// ===== ADT ожидаемых ошибок и вспомогательные средства =====
// Каждая причина неудачи — отдельный вариант типа AppError.
// Операции предметной области возвращают Either[AppError, Результат] и не бросают исключений.

import scala.util.control.NonFatal

sealed trait AppError

// ---------- Ввод пользователя ----------
sealed trait InputError extends AppError
case object UnknownCommand extends InputError
case class MissingArgument(what: String) extends InputError
case object InvalidPath extends InputError
case object InvalidPolicyName extends InputError
case object InvalidPriorityInput extends InputError
case object InvalidCountInput extends InputError
case object NoPathsSelected extends InputError

// ---------- Способ поиска ----------
sealed trait MatcherError extends AppError
case object EmptyExactValue extends MatcherError
case object EmptyRegexValue extends MatcherError
case class InvalidRegex(description: String) extends MatcherError
case class DictionaryUnreadable(file: String) extends MatcherError
case class UnknownMatcherKind(kind: String) extends MatcherError
case class UnknownBuiltinType(key: String, available: String) extends MatcherError

// ---------- Способ замены ----------
sealed trait ReplacementError extends AppError
case object ConstantValueMissing extends ReplacementError
case object PseudonymTemplateMissing extends ReplacementError
case object PseudonymTemplateWithoutNumber extends ReplacementError
case object ListValuesMissing extends ReplacementError
case object NegativeKeepCount extends ReplacementError
case class UnknownReplacementKind(kind: String) extends ReplacementError

// ---------- Правила и политики ----------
case class InvalidRule(ruleId: String, cause: AppError) extends AppError
case object RuleWithoutId extends AppError
case class DuplicateRuleId(policy: String, ruleId: String) extends AppError
case class PolicyNotFound(name: String) extends AppError
case class PolicyAlreadyExists(name: String) extends AppError
case class PolicyIncludesMissing(parent: String, missing: String) extends AppError
case class PolicyCycle(chain: List[String]) extends AppError
case class DefaultPolicyMissing(name: String) extends AppError
case class DefaultPolicyDeletion(name: String) extends AppError
case class PolicyInUse(name: String, usedBy: List[String]) extends AppError
case class PolicyRejected(name: String, problems: List[AppError]) extends AppError

// ---------- Конфигурация, журнал, отчёт ----------
case class ConfigUnreadable(path: String) extends AppError
case object ConfigMalformed extends AppError
case class ConfigFieldMissing(owner: String, field: String) extends AppError
case class ConfigFieldWrongType(owner: String, field: String) extends AppError
case class PriorityNotANumber(ruleId: String) extends AppError
case class ConfigInvalid(problems: List[AppError]) extends AppError
case object ConfigSaveFailed extends AppError
case object LogUnavailable extends AppError
case object ReportSaveFailed extends AppError

// ---------- Файлы ----------
case object OutputEqualsSource extends AppError
case object UnsupportedEncoding extends AppError
case object FileIoError extends AppError
case object SourceChanged extends AppError

// Превращает исключения внешних систем (файлы, библиотеки) в ошибки приложения.
// Исключения, для которых в handler нет варианта, не перехватываются.
object Safe {
  def apply[A](body: => A)(handler: PartialFunction[Throwable, AppError]): Either[AppError, A] =
    try {
      Right(body)
    } catch {
      case NonFatal(e) if handler.isDefinedAt(e) => Left(handler(e))
    }
}

// Операции над Either, которых нет в стандартной библиотеке
object Eithers {
  // Применяет f к каждому элементу слева направо; останавливается на первой ошибке
  def traverse[E, A, B](items: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    items
      .foldLeft[Either[E, List[B]]](Right(Nil))((acc, a) => acc.flatMap(done => f(a).map(b => b :: done)))
      .map(_.reverse)

  // Option[Either[E, A]] -> Either[E, Option[A]]
  def sequenceOption[E, A](value: Option[Either[E, A]]): Either[E, Option[A]] =
    value.fold[Either[E, Option[A]]](Right(None))(either => either.map(a => Some(a)))
}
