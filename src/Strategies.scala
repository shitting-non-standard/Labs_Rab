// ===== Паттерны Strategy и Factory =====
// Strategy: способ поиска (Matcher) и способ замены (Replacement) — взаимозаменяемые объекты.
// Factory:  MatcherFactory и ReplacementFactory создают нужный объект по настройкам из конфигурации.

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
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
    val result = mutable.ListBuffer[Hit]()
    while (m.find()) {
      if (m.end() > m.start()) { // пустые совпадения не нужны
        val groups = (1 to m.groupCount()).map(i => Option(m.group(i)).getOrElse("")).toList
        result += Hit(m.start(), m.end(), m.group(), groups)
      }
    }
    result.toList
  }
}

// Встроенные типы данных. Группа (...) нужна, чтобы сохранить имя поля: "password=" остаётся, значение заменяется.
object BuiltinPatterns {
  val all: Map[String, String] = Map(
    "email" -> """[A-Za-z0-9._%+\-]+@[A-Za-z0-9\-]+(?:\.[A-Za-z0-9\-]+)*\.[A-Za-z]{2,}""",
    "phone" -> """(?U)(?<![\w.])(?:\+7|8|\+\d{1,3})[ \-]?\(?\d{3}\)?[ \-]?\d{3}[ \-]?\d{2}[ \-]?\d{2}(?!\w)""",
    "ipv4" -> """(?<![\d.])(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)\.){3}(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(?!\.?\d)""",
    "url" -> """(?i)\b(?:https?://|www\.)[^\s<>\x22\x27()\[\]]*[^\s<>\x22\x27()\[\].,;:!?]""",
    // группа 1 = "password=", дальше идёт само значение
    "password" -> """(?iuU)\b((?:password|passwd|pwd|пароль)[\x22\x27]?[ \t]*[=:][ \t]*[\x22\x27]?)[^\s`\x22\x27,;]+""",
    // группа 1 = "token=" или "Bearer "; ключи AWS/GitHub находятся без группы
    "token" -> """(?iuU)(?:\b((?:(?:api[_-]?key|access[_-]?key|secret[_-]?key|auth[_-]?token|token|secret)[\x22\x27]?[ \t]*[=:][ \t]*[\x22\x27]?|Bearer[ \t]+))[A-Za-z0-9\-._~+/=]{6,}|\b(?:AKIA[0-9A-Z]{16}|gh[pousr]_[A-Za-z0-9]{36,})\b)"""
  )
}

// Factory: по описанию из конфигурации создаёт Matcher или сообщает об ошибке
object MatcherFactory {
  def create(spec: MatcherSpec): Matcher = spec.kind match {
    case "exact" =>
      if (spec.value.isEmpty) throw new ConfigException("для способа exact не указано значение")
      new RegexMatcher(Pattern.compile(Pattern.quote(spec.value)))

    case "regex" =>
      if (spec.value.isEmpty) throw new ConfigException("для способа regex не указано выражение")
      try {
        new RegexMatcher(Pattern.compile(spec.value))
      } catch {
        case e: PatternSyntaxException =>
          throw new ConfigException("регулярное выражение имеет неправильный формат: " + e.getDescription)
      }

    case "dictionary" => new RegexMatcher(dictionaryPattern(spec.value))

    case "builtin" =>
      val p = BuiltinPatterns.all.getOrElse(spec.value,
        throw new ConfigException(s"встроенный тип '${spec.value}' не поддерживается"))
      new RegexMatcher(Pattern.compile(p))

    case other => throw new ConfigException(s"способ поиска '$other' не поддерживается")
  }

  // Словарь: текстовый файл UTF-8, одно значение в строке; строки с # — комментарии
  private def dictionaryPattern(file: String): Pattern = {
    val words =
      try {
        Files.readAllLines(Paths.get(file), StandardCharsets.UTF_8).asScala
          .map(_.trim).filter(w => w.nonEmpty && !w.startsWith("#")).toList
      } catch {
        case _: Exception => throw new ConfigException(s"словарь '$file' отсутствует или не читается")
      }
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
}

// ---------- Псевдонимы ----------

// Запоминает, какой псевдоним выдан какому значению. Живёт одну операцию и затем пропадает,
// поэтому связь "значение -> псевдоним" после завершения не сохраняется.
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
    var seen = 0
    hit.text.map { c =>
      if (!c.isLetterOrDigit) {
        c
      } else {
        seen += 1
        if (seen <= keepStart || seen > total - keepEnd) c else '*'
      }
    }
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

// Factory для способов замены
object ReplacementFactory {
  private def need[T](option: Option[T], message: String): T =
    option.getOrElse(throw new ConfigException(message))

  def create(spec: ReplacementSpec): Replacement = spec.kind match {
    case "constant" => new Constant(need(spec.value, "для замены constant не указано значение (value)"))
    case "remove" => new Remove
    case "mask" =>
      if (spec.keepStart < 0 || spec.keepEnd < 0)
        throw new ConfigException("keepStart и keepEnd не могут быть отрицательными")
      new Mask(spec.keepStart, spec.keepEnd)
    case "pseudonym" =>
      val template = need(spec.template, "для замены pseudonym не указан шаблон (template)")
      if (!template.contains("{number}")) throw new ConfigException("шаблон pseudonym должен содержать {number}")
      new Pseudonym(template)
    case "list" =>
      if (spec.values.isEmpty) throw new ConfigException("для замены list не указан список значений (values)")
      new FromList(spec.values)
    case other => throw new ConfigException(s"способ замены '$other' не поддерживается")
  }
}

// ---------- Готовое к работе правило ----------

class Rule(val defn: RuleDef, val matcher: Matcher, val replacement: Replacement)

object Rule {
  // Бросает ConfigException, если в правиле есть ошибка
  def from(d: RuleDef): Rule =
    new Rule(d, MatcherFactory.create(d.matcher), ReplacementFactory.create(d.replacement))
}
