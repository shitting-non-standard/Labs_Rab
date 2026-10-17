// ===== Паттерн Composite: составные политики =====
// Политика хранит только ИМЕНА включённых политик (ссылки), а не копии правил.
// Перед использованием дерево политик "раскрывается" в плоский список правил.

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

// Результат раскрытия политики
case class ResolvedPolicy(
    rules: List[RuleDef],      // итоговый набор правил
    policies: List[String],    // какие политики участвовали (включая саму)
    overrides: List[String]    // сообщения о переопределённых правилах
)

object PolicyResolver {
  def resolve(name: String, all: collection.Map[String, Policy]): ResolvedPolicy = {
    val merged = mutable.LinkedHashMap[String, (RuleDef, String)]() // id правила -> (правило, из какой политики)
    val visited = mutable.LinkedHashSet[String]()
    val notes = mutable.ListBuffer[String]()

    // path — цепочка политик "сверху вниз", по ней находим циклы
    def visit(current: String, path: List[String]): Unit = {
      if (path.contains(current)) {
        Log.warn(s"Обнаружена циклическая зависимость в политике '$name'")
        throw new ConfigException("Циклическая зависимость: " + (path.reverse :+ current).mkString(" -> "))
      }
      val policy = all.getOrElse(current, throw new ConfigException(
        path.headOption.fold(s"Политика '$current' не найдена")(parent =>
          s"Политика '$parent' включает несуществующую политику '$current'")))
      visited += current
      // 1. сначала включённые политики, в указанном порядке
      policy.includes.foreach(inc => visit(inc, current :: path))
      // 2. потом собственные правила: они заменяют правила с тем же id
      for (rule <- policy.rules) {
        merged.get(rule.id) match {
          case Some((_, from)) if from != current =>
            notes += s"Правило '${rule.id}' из политики '$current' переопределяет правило из политики '$from'"
          case _ => ()
        }
        merged(rule.id) = (rule, current)
      }
    }

    visit(name, Nil)
    ResolvedPolicy(merged.values.map(_._1).toList, visited.toList, notes.toList)
  }
}

// ===== Проверка политик =====
object Validator {

  // Проверяет одну политику вместе со всеми включёнными. Пустой список = ошибок нет.
  def validatePolicy(name: String, cfg: AppConfig): List[String] =
    Try(PolicyResolver.resolve(name, cfg.policies)) match {
      case Failure(e: ConfigException) => List(e.getMessage)
      case Failure(e) => throw e
      case Success(resolved) =>
        val errors = mutable.ListBuffer[String]()

        // повторяющиеся id внутри одной политики
        for (p <- resolved.policies) {
          val ids = cfg.policies(p).rules.map(_.id)
          ids.diff(ids.distinct).distinct.foreach { id =>
            errors += s"Политика '$p': повторяется идентификатор правила '$id'"
          }
        }

        // каждое правило пробуем "собрать": так находятся ошибки поиска и замены
        for (r <- resolved.rules) {
          if (r.id.isBlank) errors += "Есть правило без идентификатора"
          Try(Rule.from(r)) match {
            case Failure(e: ConfigException) => errors += s"Правило '${r.id}': ${e.getMessage}"
            case Failure(e) => throw e
            case Success(_) => ()
          }
        }
        errors.toList
    }

  // Проверяет всю конфигурацию целиком
  def validateConfig(cfg: AppConfig): List[String] = {
    val general =
      if (cfg.policies.contains(cfg.defaultPolicy)) Nil
      else List(s"Политика по умолчанию '${cfg.defaultPolicy}' отсутствует")
    val perPolicy = cfg.policies.keys.toList.flatMap(n => validatePolicy(n, cfg))
    (general ++ perPolicy).distinct
  }
}
