// ===== Представление: данные и ошибки -> текст =====
// Только здесь ошибки и результаты превращаются в строки для пользователя.
// Предметная область работает с типами и текст не формирует.

import java.time.format.DateTimeFormatter

object Messages {

  private def list(problems: List[AppError]): String =
    problems.map(p => "\n  - " + error(p)).mkString

  // Текст для каждого варианта ошибки. Список вариантов закрыт (sealed), поэтому
  // компилятор предупредит, если добавить ошибку и забыть про текст.
  def error(e: AppError): String = e match {
    // ввод пользователя
    case UnknownCommand => "Неизвестная команда. Введите help."
    case MissingArgument(what) => s"Укажите $what"
    case InvalidPath => "Некорректный путь"
    case InvalidPolicyName => "Имя должно состоять из букв, цифр, '_' и '-'"
    case InvalidPriorityInput => "Приоритет не является числом"
    case InvalidCountInput => "Количество символов не является числом"
    case NoPathsSelected => "Не выбраны файлы или каталоги (команда select или аргументы)"
    // поиск
    case EmptyExactValue => "для способа exact не указано значение"
    case EmptyRegexValue => "для способа regex не указано выражение"
    case InvalidRegex(description) => s"регулярное выражение имеет неправильный формат: $description"
    case DictionaryUnreadable(file) => s"словарь '$file' отсутствует или не читается"
    case UnknownMatcherKind(kind) => s"способ поиска '$kind' не поддерживается"
    case UnknownBuiltinType(key, available) => s"встроенный тип '$key' не поддерживается (доступны: $available)"
    // замена
    case ConstantValueMissing => "для замены constant не указано значение (value)"
    case PseudonymTemplateMissing => "для замены pseudonym не указан шаблон (template)"
    case PseudonymTemplateWithoutNumber => "шаблон pseudonym должен содержать {number}"
    case ListValuesMissing => "для замены list не указан список значений (values)"
    case NegativeKeepCount => "keepStart и keepEnd не могут быть отрицательными"
    case UnknownReplacementKind(kind) => s"способ замены '$kind' не поддерживается"
    // правила и политики
    case InvalidRule(id, cause) => s"Правило '$id': ${error(cause)}"
    case RuleWithoutId => "Есть правило без идентификатора"
    case DuplicateRuleId(policy, id) => s"Политика '$policy': повторяется идентификатор правила '$id'"
    case PolicyNotFound(name) => s"Политика '$name' не найдена"
    case PolicyAlreadyExists(_) => "Политика с таким именем уже есть"
    case PolicyIncludesMissing(parent, missing) => s"Политика '$parent' включает несуществующую политику '$missing'"
    case PolicyCycle(chain) => "Циклическая зависимость: " + chain.mkString(" -> ")
    case DefaultPolicyMissing(name) => s"Политика по умолчанию '$name' отсутствует"
    case DefaultPolicyDeletion(_) => "Нельзя удалить политику по умолчанию"
    case PolicyInUse(_, usedBy) => s"Политика входит в состав: ${usedBy.mkString(", ")}"
    case PolicyRejected(name, problems) => s"Политика '$name' некорректна:" + list(problems)
    // конфигурация, журнал, отчёт
    case ConfigUnreadable(path) => s"Не удалось прочитать файл конфигурации '$path'"
    case ConfigMalformed => "Конфигурационный файл имеет неправильный формат"
    case ConfigFieldMissing(owner, field) => s"$owner: нет поля '$field'"
    case ConfigFieldWrongType(owner, field) => s"$owner: поле '$field' имеет неправильный тип"
    case PriorityNotANumber(id) => s"Правило '$id': приоритет не является числом"
    case ConfigInvalid(problems) => "Конфигурация содержит ошибки:" + list(problems)
    case ConfigSaveFailed => "Не удалось сохранить конфигурацию"
    case LogUnavailable => "не удалось создать журнал, работа продолжится без него"
    case ReportSaveFailed => "Не удалось сохранить отчёт в файл"
    // файлы
    case OutputEqualsSource => "Выходной каталог совпадает с исходным"
    case UnsupportedEncoding => "файл содержит данные в неподдерживаемой кодировке"
    case FileIoError => "ошибка чтения или записи файла"
    case SourceChanged => "исходный файл изменился во время обработки"
  }

  def skipped(s: Skipped): String = s.reason match {
    case PathMissing => s"пропущен ${s.name}: файл или каталог не найден"
    case UnsupportedExtension => s"пропущен ${s.name}: неподдерживаемое расширение"
  }

  def overrideText(o: RuleOverride): String =
    s"Правило '${o.ruleId}' из политики '${o.by}' переопределяет правило из политики '${o.replaced}'"

  def label(r: RuleDef): String = r.description.getOrElse(r.id)

  private def matcherLabel(spec: MatcherSpec): String = spec match {
    case ExactSpec(value) => s"exact:$value"
    case RegexSpec(pattern) => s"regex:$pattern"
    case DictionarySpec(path) => s"dictionary:$path"
    case BuiltinSpec(builtin) => s"builtin:${builtin.key}"
  }

  private def replacementLabel(spec: ReplacementSpec): String = spec match {
    case _: ConstantSpec => "constant"
    case RemoveSpec => "remove"
    case _: MaskSpec => "mask"
    case _: PseudonymSpec => "pseudonym"
    case _: ListSpec => "list"
  }

  def describeRule(r: RuleDef): String = {
    val state = if (r.enabled) "включено" else "выключено"
    s"${r.id} [приоритет ${r.priority}, $state] поиск ${matcherLabel(r.matcher)} -> замена ${replacementLabel(r.replacement)}"
  }

  def report(r: Report): String = {
    val fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val head = List(
      if (r.complete) "Обработка завершена." else "Обработка завершена, РЕЗУЛЬТАТ НЕПОЛНЫЙ.",
      s"Политика: ${r.policy}",
      s"Включённые политики: ${r.included.mkString(", ")}")
    val overrides = r.overrides.map(o => "Переопределение: " + overrideText(o))
    val body = List(
      s"Начало: ${r.started.format(fmt)}",
      s"Завершение: ${r.finished.format(fmt)}",
      s"Обработано файлов: ${r.processed}",
      s"Пропущено файлов: ${r.skipped}",
      s"Не перезаписано (уже существуют): ${r.kept.size}",
      s"Создано замен: ${r.replacements.map(_._2).sum}")
    val perRule = r.replacements.map { case (id, n) => s"  правило $id: $n" }
    val tail = List(s"Конфликтов: ${r.conflicts}", s"Ошибок: ${r.errors.size}")
    val failures = r.errors.map(f => s"  ${f.file}: ${error(f.error)}")
    val kept = r.kept.map(name => s"  не перезаписан: $name")
    val footer = List("Результат сохранён:", r.outputDir)
    (head ++ overrides ++ body ++ perRule ++ tail ++ failures ++ kept ++ footer).mkString("\n")
  }
}
