// ===== Политики: раскрытие, проверка, операции =====
// Всё здесь — чистые функции: они принимают данные и возвращают данные или ошибку.
// Вывода на экран, чтения ввода и записи в журнал здесь нет.

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

// ---------- Паттерн Composite: составные политики ----------
// Политика хранит только ИМЕНА включённых политик (ссылки), а не копии правил.
// Перед использованием дерево политик "раскрывается" в плоский список правил.

// Правило ruleId из политики `by` переопределило правило из политики `replaced`
case class RuleOverride(ruleId: String, by: String, replaced: String)

// Результат раскрытия политики
case class ResolvedPolicy(
    rules: List[RuleDef],      // итоговый набор правил
    policies: List[String],    // какие политики участвовали (включая саму)
    overrides: List[RuleOverride]  // какие правила были переопределены
)

object PolicyResolver {
  // Накапливаемое состояние обхода (неизменяемое)
  private case class Acc(
      merged: ListMap[String, (RuleDef, String)], // id правила -> (правило, из какой политики)
      visited: List[String],
      overrides: List[RuleOverride])

  def resolve(name: String, all: collection.Map[String, Policy]): Either[AppError, ResolvedPolicy] =
    visit(name, Nil, Acc(ListMap.empty, Nil, Nil), all).map { acc =>
      ResolvedPolicy(acc.merged.values.map(_._1).toList, acc.visited.distinct, acc.overrides)
    }

  // path — цепочка политик "сверху вниз"; по ней находятся циклы
  private def visit(
      current: String,
      path: List[String],
      acc: Acc,
      all: collection.Map[String, Policy]): Either[AppError, Acc] =
    if (path.contains(current)) {
      Left(PolicyCycle((path.reverse :+ current)))
    } else {
      all.get(current).toRight(missing(current, path)).flatMap { policy =>
        val start: Either[AppError, Acc] = Right(acc.copy(visited = acc.visited :+ current))
        // 1. сначала включённые политики, в указанном порядке
        val withIncludes = policy.includes.foldLeft(start) { (accE, inc) =>
          accE.flatMap(a => visit(inc, current :: path, a, all))
        }
        // 2. потом собственные правила: они заменяют правила с тем же id
        withIncludes.map(a => policy.rules.foldLeft(a)((state, rule) => addRule(state, rule, current)))
      }
    }

  private def missing(current: String, path: List[String]): AppError =
    path.headOption.fold[AppError](PolicyNotFound(current))(parent => PolicyIncludesMissing(parent, current))

  private def addRule(acc: Acc, rule: RuleDef, current: String): Acc = {
    val replaced = acc.merged.get(rule.id).collect {
      case (_, from) if from != current => RuleOverride(rule.id, current, from)
    }
    acc.copy(merged = acc.merged.updated(rule.id, (rule, current)), overrides = acc.overrides ++ replaced)
  }
}

// ---------- Проверка ----------
object Validator {

  // Все проблемы политики и включённых в неё политик. Пустой список = ошибок нет.
  def validatePolicy(name: String, cfg: AppConfig): List[AppError] =
    PolicyResolver.resolve(name, cfg.policies) match {
      case Left(error) => List(error)
      case Right(resolved) =>
        val duplicates = resolved.policies.flatMap { p =>
          cfg.policies.get(p).toList.flatMap(policy => duplicateIds(p, policy.rules))
        }
        val withoutId = resolved.rules.filter(_.id.isBlank).map(_ => RuleWithoutId)
        // каждое правило пробуем "собрать": так находятся ошибки поиска и замены
        val broken = resolved.rules.flatMap(r => Rule.from(r).left.toOption)
        duplicates ++ withoutId ++ broken
    }

  private def duplicateIds(policy: String, rules: List[RuleDef]): List[AppError] = {
    val ids = rules.map(_.id)
    ids.diff(ids.distinct).distinct.map(id => DuplicateRuleId(policy, id))
  }

  def ensureValid(name: String, cfg: AppConfig): Either[AppError, Unit] =
    validatePolicy(name, cfg) match {
      case Nil => Right(())
      case problems => Left(PolicyRejected(name, problems))
    }

  // Проверка всей конфигурации
  def validateConfig(cfg: AppConfig): List[AppError] = {
    val general =
      if (cfg.policies.contains(cfg.defaultPolicy)) Nil
      else List(DefaultPolicyMissing(cfg.defaultPolicy))
    (general ++ cfg.policies.keys.toList.flatMap(n => validatePolicy(n, cfg))).distinct
  }

  def ensureConfigValid(cfg: AppConfig): Either[AppError, Unit] =
    validateConfig(cfg) match {
      case Nil => Right(())
      case problems => Left(ConfigInvalid(problems))
    }
}

// ---------- Операции над политиками ----------
object PolicyOps {
  def validateName(name: String): Either[AppError, String] =
    Either.cond(name.matches("[\\p{L}\\p{N}_\\-]+"), name, InvalidPolicyName)

  def find(cfg: AppConfig, name: String): Either[AppError, Policy] =
    cfg.policies.get(name).toRight(PolicyNotFound(name))

  def ensureNew(cfg: AppConfig, name: String): Either[AppError, String] =
    Either.cond(!cfg.policies.contains(name), name, PolicyAlreadyExists(name))

  // Новая конфигурация с добавленной или заменённой политикой. Политика должна быть корректной.
  def put(cfg: AppConfig, name: String, policy: Policy): Either[AppError, AppConfig] = {
    val updated = cfg.copy(policies = cfg.policies.updated(name, policy))
    Validator.ensureValid(name, updated).map(_ => updated)
  }

  // Новая конфигурация без политики. Нельзя удалить политику по умолчанию или включённую в другую.
  def remove(cfg: AppConfig, name: String): Either[AppError, AppConfig] =
    for {
      _ <- find(cfg, name)
      _ <- Either.cond(name != cfg.defaultPolicy, (), DefaultPolicyDeletion(name))
      usedBy = cfg.policies.collect { case (n, p) if p.includes.contains(name) => n }.toList
      _ <- Either.cond(usedBy.isEmpty, (), PolicyInUse(name, usedBy))
    } yield cfg.copy(policies = cfg.policies.removed(name))

  // Идентификаторы, уже занятые в конфигурации, плюс ещё не сохранённые правила
  def takenIds(cfg: AppConfig, extra: List[RuleDef]): Set[String] =
    cfg.policies.values.flatMap(p => p.rules.map(_.id)).toSet ++ extra.map(_.id)

  def addRule(policy: Policy, rule: RuleDef): Policy = policy.copy(rules = policy.rules :+ rule)

  def removeRule(policy: Policy, id: String): Policy = policy.copy(rules = policy.rules.filterNot(_.id == id))

  def toggleRule(policy: Policy, id: String): Policy =
    policy.copy(rules = policy.rules.map(r => if (r.id == id) r.copy(enabled = !r.enabled) else r))

  def describe(policy: Policy, text: Option[String]): Policy = policy.copy(description = text)
}

// ---------- Создание правила ----------

// Дополнительные ответы о способе замены. Что именно нужно, зависит от вида замены;
// всё, чего пользователь не указал, — None или пустой список.
case class ReplacementDetails(
    value: Option[String] = None,
    template: Option[String] = None,
    values: List[String] = Nil,
    keepStart: Option[Int] = None,
    keepEnd: Option[Int] = None)

// Ответы пользователя на вопросы о правиле: виды уже разобраны в ADT
case class RuleDraft(
    description: Option[String],
    matcherKind: MatcherKind,
    matcherValue: String,
    replacementKind: ReplacementKind,
    details: ReplacementDetails,
    priority: Option[Int])

object RuleBuilder {
  // Превращает ответы в правило: разбирает способ поиска и замены, проверяет приоритет, создаёт
  // идентификатор и сразу проверяет, что правило собирается (например, выражение корректно).
  // Не указанное число символов для маски означает "0 символов": это явное значение по умолчанию.
  def build(draft: RuleDraft, taken: Set[String]): Either[AppError, RuleDef] =
    for {
      matcher <- MatcherSpec.parse(draft.matcherKind, draft.matcherValue)
      replacement <- ReplacementSpec.parse(
        draft.replacementKind,
        draft.details.value,
        draft.details.template,
        draft.details.values,
        draft.details.keepStart.getOrElse(0),
        draft.details.keepEnd.getOrElse(0))
      priority <- draft.priority.toRight(InvalidPriorityInput)
      rule = RuleDef(RuleIds.generate(matcher, taken), draft.description, matcher, replacement, priority, enabled = true)
      _ <- Rule.from(rule)
    } yield rule
}

// Автоматические идентификаторы правил: пользователь их не вводит.
// Идентификатор строится из способа поиска: builtin-email, dictionary-people, regex, exact.
// Содержимое exact и regex в идентификатор не попадает: оно может быть чувствительным,
// а идентификаторы пишутся в журнал.
object RuleIds {
  def generate(matcher: MatcherSpec, taken: Set[String]): String = {
    val base = matcher match {
      case BuiltinSpec(builtin) => "builtin-" + slug(builtin.key)
      case DictionarySpec(path) => "dictionary-" + slug(fileStem(path))
      case _: ExactSpec => "exact"
      case _: RegexSpec => "regex"
    }
    if (!taken.contains(base)) base else free(base, 2, taken)
  }

  // Первый свободный идентификатор base-2, base-3, ... (функция полная: свободный номер всегда найдётся)
  @tailrec
  private def free(base: String, n: Int, taken: Set[String]): String = {
    val id = s"$base-$n"
    if (taken.contains(id)) free(base, n + 1, taken) else id
  }

  // Имя файла без каталога и расширения
  private def fileStem(path: String): String =
    path.split("[/\\\\]").lastOption.getOrElse("").replaceAll("\\.[^.]*$", "")

  private def slug(text: String): String =
    text.toLowerCase.replaceAll("[^\\p{L}\\p{N}]+", "-").stripPrefix("-").stripSuffix("-")
}
