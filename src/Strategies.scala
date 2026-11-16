// ===== Паттерны Strategy и Factory =====
// Strategy: способ поиска (Matcher) и способ замены (Replacement) — взаимозаменяемые объекты.
// Factory:  MatcherFactory и ReplacementFactory создают нужный объект по ADT-описанию (MatcherSpec, ReplacementSpec).

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.regex.{Pattern, PatternSyntaxException}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

// ---------- Найденное совпадение ----------

// start/end — позиции в тексте, groups — значения групп регулярного выражения ($1, $2 ...)
case class Hit(start: Int, end: Int, text: String, groups: List[String])

// ---------- Strategy: способы поиска ----------

trait Matcher {
  def find(text: String): List[Hit]
}

// Все способы поиска (exact, regex, словарь, встроенные типы) сводятся к регулярному выражению
class RegexMatcher(pattern: Pattern) extends Matcher {
  def find(text: String): List[Hit] = {
    val m = pattern.matcher(text)
    // Итератор вызывает find() и сразу читает найденное совпадение; пустые совпадения пропускаются
    Iterator.continually(m)
      .takeWhile(x => x.find())
      .filter(x => x.end() > x.start())
      .map { x =>
        val groups = (1 to x.groupCount()).map(i => Option(x.group(i)).getOrElse("")).toList
        Hit(x.start(), x.end(), x.group(), groups)
      }
      .toList
  }
}

// Встроенные типы данных. Каждый тип — значение перечисления: ключ (его пишут в конфигурации),
// описание и регулярное выражение. Группа (...) в выражении нужна, чтобы сохранить имя поля
// (например, "password="): заменяется только значение.
enum BuiltinType(val key: String, val description: String, val regex: String) {
  case Email extends BuiltinType("email", "Адрес электронной почты",
    """[A-Za-z0-9._%+\-]+@[A-Za-z0-9\-]+(?:\.[A-Za-z0-9\-]+)*\.[A-Za-z]{2,}""")
  case Phone extends BuiltinType("phone", "Телефонный номер",
    """(?U)(?<![\w.])(?:\+7|8|\+\d{1,3})[ \-]?\(?\d{3}\)?[ \-]?\d{3}[ \-]?\d{2}[ \-]?\d{2}(?!\w)""")
  case IPv4 extends BuiltinType("ipv4", "IPv4-адрес",
    """(?<![\d.])(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)\.){3}(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(?!\.?\d)""")
  case Url extends BuiltinType("url", "Ссылка (URL)",
    """(?i)\b(?:https?://|www\.)[^\s<>\x22\x27()\[\]]*[^\s<>\x22\x27()\[\].,;:!?]""")
  // группа 1 = "password=", дальше идёт само значение
  case Password extends BuiltinType("password", "Пароль в формате поле=значение",
    """(?iuU)\b((?:password|passwd|pwd|пароль)[\x22\x27]?[ \t]*[=:][ \t]*[\x22\x27]?)[^\s`\x22\x27,;]+""")
  // группа 1 = "token=" или "Bearer "; ключи AWS и GitHub находятся без группы
  case Token extends BuiltinType("token", "Токен или ключ доступа",
    """(?iuU)(?:\b((?:(?:api[_-]?key|access[_-]?key|secret[_-]?key|auth[_-]?token|token|secret)[\x22\x27]?[ \t]*[=:][ \t]*[\x22\x27]?|Bearer[ \t]+))[A-Za-z0-9\-._~+/=]{6,}|\b(?:AKIA[0-9A-Z]{16}|gh[pousr]_[A-Za-z0-9]{36,})\b)""")

  // Выражение компилируется один раз, при первом обращении
  lazy val pattern: Pattern = Pattern.compile(regex)
}

object BuiltinType {
  def byKey(key: String): Option[BuiltinType] = values.find(t => t.key == key)

  // Ключи для подсказок: email/phone/ipv4/...
  def keys: String = values.map(_.key).mkString("/")
}

// Factory: по ADT-описанию создаёт Matcher. Ошибкой может закончиться только компиляция
// регулярного выражения и чтение словаря — это возвращается как значение.
object MatcherFactory {
  def create(spec: MatcherSpec): Either[AppError, Matcher] = spec match {
    case ExactSpec(value) => Right(new RegexMatcher(Pattern.compile(Pattern.quote(value))))
    case RegexSpec(pattern) =>
      Safe(new RegexMatcher(Pattern.compile(pattern))) {
        case e: PatternSyntaxException => InvalidRegex(e.getDescription)
      }
    case DictionarySpec(path) => dictionaryPattern(path).map(p => new RegexMatcher(p))
    case BuiltinSpec(builtin) => Right(new RegexMatcher(builtin.pattern))
  }

  // Словарь: текстовый файл UTF-8, одно значение в строке; строки с # — комментарии
  private def dictionaryPattern(file: String): Either[AppError, Pattern] = {
    val words: Either[AppError, List[String]] = Safe {
      Files.readAllLines(Workspace.resolve(file), StandardCharsets.UTF_8).asScala
        .map(_.trim).filter(w => w.nonEmpty && !w.startsWith("#")).toList
    } {
      case _: Exception => DictionaryUnreadable(file)
    }
    words.map(compileDictionary)
  }

  private def compileDictionary(words: List[String]): Pattern =
    if (words.isEmpty) {
      Pattern.compile("(?!)") // пустой словарь ничего не находит
    } else {
      // длинные значения первыми; слово должно быть отдельным (не частью другого слова)
      val alternatives = words.sortBy(w => -w.length).map(w => Pattern.quote(w)).mkString("|")
      Pattern.compile(
        """(?<![\p{L}\p{N}_])(?:""" + alternatives + """)(?![\p{L}\p{N}_])""",
        Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE)
    }
}

// ---------- Псевдонимы ----------

// Запоминает, какой псевдоним выдан какому значению. Живёт одну операцию и затем пропадает,
// поэтому связь "значение -> псевдоним" после завершения не сохраняется.
// Единственный изменяемый объект в предметной области: счётчики псевдонимов.
class PseudonymBook {
  private val known = mutable.Map[(String, String), String]() // (id правила, значение) -> псевдоним
  private val counters = mutable.Map[String, Int]()           // id правила -> сколько выдано

  def pseudonym(ruleId: String, original: String)(make: Int => String): String =
    known.getOrElseUpdate((ruleId, original), {
      val n = counters.getOrElse(ruleId, 0) + 1
      counters(ruleId) = n
      make(n)
    })
}

// ---------- Strategy: способы замены ----------

trait Replacement {
  def apply(hit: Hit, ruleId: String, book: PseudonymBook): String
}

// Постоянная строка. Можно ссылаться на группы: "$1KEY" -> значение группы 1 + "KEY"
class Constant(value: String) extends Replacement {
  def apply(hit: Hit, ruleId: String, book: PseudonymBook): String =
    hit.groups.zipWithIndex.reverse.foldLeft(value) { case (acc, (group, i)) =>
      acc.replace("$" + (i + 1), group)
    }
}

// Удаление
class Remove extends Replacement {
  def apply(hit: Hit, ruleId: String, book: PseudonymBook): String = ""
}

// Частичное маскирование: буквы и цифры -> '*', знаки препинания остаются.
// keepStart/keepEnd — сколько букв/цифр оставить в начале/конце.
class Mask(keepStart: Int, keepEnd: Int) extends Replacement {
  def apply(hit: Hit, ruleId: String, book: PseudonymBook): String = {
    val total = hit.text.count(_.isLetterOrDigit)
    // для каждого символа: сколько букв/цифр было до него включительно
    val seen = hit.text.scanLeft(0)((n, c) => if (c.isLetterOrDigit) n + 1 else n).tail
    hit.text.zip(seen).map { case (c, n) =>
      if (!c.isLetterOrDigit || n <= keepStart || n > total - keepEnd) c else '*'
    }.mkString
  }
}

// Последовательные псевдонимы: user1@example.com, user2@example.com ...
class Pseudonym(template: String) extends Replacement {
  def apply(hit: Hit, ruleId: String, book: PseudonymBook): String =
    book.pseudonym(ruleId, hit.text)(n => template.replace("{number}", n.toString))
}

// Значения из списка по очереди. Когда список кончился, к значению добавляется номер круга.
class FromList(values: List[String]) extends Replacement {
  def apply(hit: Hit, ruleId: String, book: PseudonymBook): String =
    book.pseudonym(ruleId, hit.text) { n =>
      val i = n - 1
      val base = values(i % values.size)
      if (i < values.size) base else base + (i / values.size + 1)
    }
}

// Factory для способов замены. Все параметры уже проверены при создании ReplacementSpec,
// поэтому функция полная и ошибок не возвращает.
object ReplacementFactory {
  def create(spec: ReplacementSpec): Replacement = spec match {
    case ConstantSpec(value) => new Constant(value)
    case RemoveSpec => new Remove
    case MaskSpec(keepStart, keepEnd) => new Mask(keepStart, keepEnd)
    case PseudonymSpec(template) => new Pseudonym(template)
    case ListSpec(values) => new FromList(values)
  }
}

// ---------- Готовое к работе правило ----------

class Rule(val defn: RuleDef, val matcher: Matcher, val replacement: Replacement)

object Rule {
  // Ошибка, найденная при сборке, помечается идентификатором правила
  def from(d: RuleDef): Either[AppError, Rule] =
    MatcherFactory.create(d.matcher)
      .map(m => new Rule(d, m, ReplacementFactory.create(d.replacement)))
      .left.map(cause => InvalidRule(d.id, cause))
}
