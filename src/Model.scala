// ===== Модель данных (ADT) =====
// Здесь только описание структур и разбор их из "сырых" значений. Ввода-вывода нет.

import scala.collection.immutable.ListMap

// ---------- Виды способов поиска и замены (то, что пользователь выбирает по имени) ----------

enum MatcherKind(val key: String) {
  case Exact extends MatcherKind("exact")
  case Regex extends MatcherKind("regex")
  case Dictionary extends MatcherKind("dictionary")
  case Builtin extends MatcherKind("builtin")
}

object MatcherKind {
  def parse(text: String): Either[AppError, MatcherKind] =
    values.find(k => k.key == text).toRight(UnknownMatcherKind(text))

  def keys: String = values.map(_.key).mkString("/")
}

enum ReplacementKind(val key: String) {
  case Constant extends ReplacementKind("constant")
  case Remove extends ReplacementKind("remove")
  case Mask extends ReplacementKind("mask")
  case Pseudonym extends ReplacementKind("pseudonym")
  case Values extends ReplacementKind("list")
}

object ReplacementKind {
  def parse(text: String): Either[AppError, ReplacementKind] =
    values.find(k => k.key == text).toRight(UnknownReplacementKind(text))

  def keys: String = values.map(_.key).mkString("/")
}

// ---------- Способ поиска ----------
sealed trait MatcherSpec
case class ExactSpec(value: String) extends MatcherSpec
case class RegexSpec(pattern: String) extends MatcherSpec
case class DictionarySpec(path: String) extends MatcherSpec
case class BuiltinSpec(builtin: BuiltinType) extends MatcherSpec

object MatcherSpec {
  // Из вида и значения. Сопоставление по виду исчерпывающее: неизвестного вида здесь быть не может.
  def parse(kind: MatcherKind, value: String): Either[AppError, MatcherSpec] = kind match {
    case MatcherKind.Exact => Either.cond(value.nonEmpty, ExactSpec(value), EmptyExactValue)
    case MatcherKind.Regex => Either.cond(value.nonEmpty, RegexSpec(value), EmptyRegexValue)
    case MatcherKind.Dictionary => Right(DictionarySpec(value))
    case MatcherKind.Builtin =>
      BuiltinType.byKey(value)
        .map(b => BuiltinSpec(b))
        .toRight(UnknownBuiltinType(value, BuiltinType.keys))
  }
}

// ---------- Способ замены ----------
sealed trait ReplacementSpec
case class ConstantSpec(value: String) extends ReplacementSpec
case object RemoveSpec extends ReplacementSpec
case class MaskSpec(keepStart: Int, keepEnd: Int) extends ReplacementSpec
case class PseudonymSpec(template: String) extends ReplacementSpec
case class ListSpec(values: List[String]) extends ReplacementSpec // список не пустой

object ReplacementSpec {
  // Необязательные поля приходят как Option: отсутствие значения становится ошибкой, а не пустой строкой
  def parse(
      kind: ReplacementKind,
      value: Option[String],
      template: Option[String],
      values: List[String],
      keepStart: Int,
      keepEnd: Int): Either[AppError, ReplacementSpec] = kind match {
    case ReplacementKind.Constant => value.map(v => ConstantSpec(v)).toRight(ConstantValueMissing)
    case ReplacementKind.Remove => Right(RemoveSpec)
    case ReplacementKind.Mask =>
      Either.cond(keepStart >= 0 && keepEnd >= 0, MaskSpec(keepStart, keepEnd), NegativeKeepCount)
    case ReplacementKind.Pseudonym =>
      template
        .toRight(PseudonymTemplateMissing)
        .flatMap(t => Either.cond(t.contains("{number}"), PseudonymSpec(t), PseudonymTemplateWithoutNumber))
    case ReplacementKind.Values => Either.cond(values.nonEmpty, ListSpec(values), ListValuesMissing)
  }
}

// ---------- Правила, политики, конфигурация ----------

// Правило анонимизации (как оно записано в конфигурации)
case class RuleDef(
    id: String,
    description: Option[String], // None — описания нет
    matcher: MatcherSpec,
    replacement: ReplacementSpec,
    priority: Int,
    enabled: Boolean
)

// Политика: ссылки на другие политики (includes) + собственные правила
case class Policy(description: Option[String], includes: List[String], rules: List[RuleDef])

// Вся конфигурация приложения
case class AppConfig(
    logFile: String,
    reportDirectory: String,
    outputDirectory: String,
    defaultPolicy: String,
    preserveDirectoryStructure: Boolean,
    overwriteExistingFiles: Boolean,
    supportedExtensions: List[String],
    policies: ListMap[String, Policy] // ListMap хранит порядок, как в файле
)
