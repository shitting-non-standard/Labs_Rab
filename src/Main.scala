//> using scala 3.10.0
//> using dep tools.jackson.core:jackson-databind:3.2.1

// ===== Менеджер политик анонимизации: консольный интерфейс =====
// Запуск: scala-cli run . -- anonymizer.json  или  sbt run (см. build.sbt)

import java.nio.file.{Files, Path, Paths}
import java.time.format.DateTimeFormatter
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object Main {
  def main(args: Array[String]): Unit = {
    val configPath = Paths.get(args.headOption.getOrElse("anonymizer.json")).toAbsolutePath
    Workspace.baseDir = configPath.getParent // пути из конфигурации считаем от её папки
    val config =
      try {
        ConfigStore.loadValidated(configPath)
      } catch {
        case e: ConfigException =>
          println("Ошибка конфигурации:\n" + e.getMessage)
          sys.exit(1)
      }
    Log.init(config.logFile)
    Log.info("Запуск приложения")
    Log.info("Конфигурация загружена и проверена")
    new App(configPath, config).run()
    Log.info("Завершение работы приложения")
  }
}

class App(configPath: Path, initialConfig: AppConfig) {
  private var config = initialConfig
  private var selection: List[Path] = Nil        // пути, выбранные командой select
  private var selectionRecursive = false
  private var lastReport: Option[Report] = None
  private var previewed: Option[String] = None  // для каких политики и файлов уже выполнялся preview

  // Разбор строки на слова; текст в кавычках считается одним словом (пути с пробелами)
  private val tokenRe = "\"([^\"]*)\"|(\\S+)".r

  // Всё, что нужно для предпросмотра и обработки
  private case class Prepared(
      policy: String,
      resolved: ResolvedPolicy,
      rules: List[Rule],
      scan: Scan,
      outcomes: List[FileOutcome],
      sources: String,
      key: String)

  // ---------- Главный цикл ----------

  def run(): Unit = {
    println("Менеджер политик анонимизации. Команда help — список команд.")
    var running = true
    while (running) {
      val line = scala.io.StdIn.readLine("> ")
      if (line == null) {
        running = false // конец ввода
      } else {
        tokenize(line) match {
          case Nil => ()
          case "exit" :: _ => running = false
          case name :: rest => execute(name, rest)
        }
      }
    }
  }

  // Диспетчер: имя команды -> метод. Ошибки одной команды не роняют приложение.
  private def execute(name: String, args: List[String]): Unit = {
    try {
      name match {
        case "help" => help()
        case "policies" => listPolicies()
        case "show-policy" => showPolicy(args)
        case "create-policy" => createPolicy()
        case "edit-policy" => editPolicy(args)
        case "delete-policy" => deletePolicy(args)
        case "compose-policy" => composePolicy(args)
        case "validate-policy" => validatePolicy(args)
        case "select" => select(args)
        case "preview" => preview(args)
        case "anonymize" => anonymize(args)
        case "report" => showReport()
        case "reload" => reload()
        case _ =>
          println("Неизвестная команда. Введите help.")
          Log.warn("Неизвестная команда") // текст команды в журнал не пишем
      }
    } catch {
      case e: ConfigException => println("Ошибка: " + e.getMessage)
      case e: Exception =>
        println("Внутренняя ошибка приложения.")
        Log.error("Внутренняя ошибка: " + e.getClass.getSimpleName)
    }
  }

  private def tokenize(line: String): List[String] =
    tokenRe.findAllMatchIn(line).map(m => Option(m.group(1)).getOrElse(m.group(2))).toList

  private def ask(prompt: String): String = {
    val line = scala.io.StdIn.readLine(prompt)
    if (line == null) "" else line.trim
  }

  // Переспрашивает, пока не получит понятный ответ. Если ввод закрыт — считаем отказом.
  private def confirm(question: String): Boolean = {
    var answer: Option[Boolean] = None
    while (answer.isEmpty) {
      val line = scala.io.StdIn.readLine(question + " (y/n): ")
      if (line == null) {
        println("Ввод закрыт, действие отменено.")
        answer = Some(false)
      } else {
        line.trim.toLowerCase match {
          case "y" | "yes" | "д" | "да" => answer = Some(true)
          case "n" | "no" | "н" | "нет" => answer = Some(false)
          case _ => println("Введите y или n.")
        }
      }
    }
    answer.get
  }

  private def argument(args: List[String], what: String): String =
    args.headOption.getOrElse(throw new ConfigException(s"Укажите $what"))

  private def policyOrFail(name: String): Policy =
    config.policies.getOrElse(name, throw new ConfigException(s"Политика '$name' не найдена"))

  // ---------- Команды: справка и просмотр политик ----------

  private def help(): Unit =
    println("""Команды:
      |  policies                           список политик
      |  show-policy <имя>                  правила политики
      |  create-policy                      создать политику
      |  edit-policy <имя>                  изменить политику
      |  delete-policy <имя>                удалить политику
      |  compose-policy <имя> [политики]    составить политику из существующих
      |  validate-policy <имя>              проверить политику
      |  select [-r] <путь>...              выбрать файлы/каталоги (-r — с вложенными)
      |  preview <политика> [-r] [путь]...  предпросмотр найденных данных
      |  anonymize <политика> [-r] [путь]... создать анонимизированные копии
      |  report                             последний отчёт
      |  reload                             перечитать конфигурацию
      |  exit                               выход""".stripMargin)

  private def listPolicies(): Unit = {
    config.policies.foreach { case (name, p) =>
      val inc = if (p.includes.nonEmpty) s" (включает: ${p.includes.mkString(", ")})" else ""
      val star = if (name == config.defaultPolicy) " *" else ""
      println(s"  $name$star — ${p.description}$inc")
    }
    println("(* — политика по умолчанию)")
  }

  private def describeRule(r: RuleDef): String = {
    val state = if (r.enabled) "включено" else "выключено"
    s"${r.id} [приоритет ${r.priority}, $state] поиск ${r.matcher.kind}:${r.matcher.value} -> замена ${r.replacement.kind}"
  }

  private def showPolicy(args: List[String]): Unit = {
    val name = argument(args, "имя политики")
    val p = policyOrFail(name)
    println(s"Политика: $name\nОписание: ${p.description}")
    if (p.includes.nonEmpty) println("Включает: " + p.includes.mkString(", "))
    println("Собственные правила:")
    p.rules.foreach(r => println("  " + describeRule(r)))
    Try(PolicyResolver.resolve(name, config.policies)).foreach { res =>
      println("Итоговый набор правил: " + res.rules.map(_.id).mkString(", "))
      res.overrides.foreach(o => println("  ! " + o))
    }
  }

  // ---------- Команды: изменение политик ----------

  // Сохраняет политику, только если она корректна. Иначе конфигурация не меняется.
  private def commit(name: String, policy: Policy, action: String): Unit = {
    val updated = config.copy(policies = config.policies.updated(name, policy))
    val errors = Validator.validatePolicy(name, updated)
    if (errors.nonEmpty) {
      println("Политика не сохранена, найдены ошибки:")
      errors.foreach(e => println("  - " + e))
      Log.warn(s"Политика '$name' не прошла проверку, ошибок: ${errors.size}")
    } else {
      ConfigStore.save(configPath, updated) // при ошибке сохранения бросит исключение, config не изменится
      config = updated
      previewed = None
      println(s"Политика '$name' сохранена.")
      Log.info(s"$action политики '$name'")
      reportOverrides(name)
    }
  }

  private def reportOverrides(name: String): Unit =
    Try(PolicyResolver.resolve(name, config.policies)).foreach { res =>
      res.overrides.foreach { o =>
        println("Переопределение: " + o)
        Log.info("Переопределение: " + o) // только имена политик и id правил
      }
    }

  private def checkName(name: String): Unit =
    if (!name.matches("[\\p{L}\\p{N}_\\-]+"))
      throw new ConfigException("Имя должно состоять из букв, цифр, '_' и '-'")

  // Опрос пользователя: одно правило. Ввод не логируется.
  private def askRule(): RuleDef = {
    val id = ask("id правила: ")
    val description = ask("Описание: ")
    val matcherKind = ask("Способ поиска (exact/regex/dictionary/builtin): ")
    val prompt = matcherKind match {
      case "dictionary" => "Путь к файлу словаря: "
      case "builtin" => "Тип (email/phone/ipv4/url/password/token): "
      case _ => "Значение или выражение: "
    }
    val matcherValue = ask(prompt)

    val replKind = ask("Способ замены (constant/remove/mask/pseudonym/list): ")
    val replacement = replKind match {
      case "constant" =>
        ReplacementSpec(replKind, value = Some(ask("Строка замены: ")).filter(_.nonEmpty))
      case "pseudonym" =>
        ReplacementSpec(replKind, template = Some(ask("Шаблон, например user{number}@example.com: ")).filter(_.nonEmpty))
      case "list" =>
        ReplacementSpec(replKind, values = ask("Значения через ';': ").split(";").map(_.trim).filter(_.nonEmpty).toList)
      case "mask" =>
        ReplacementSpec(replKind,
          keepStart = ask("Сколько символов оставить в начале: ").toIntOption.getOrElse(0),
          keepEnd = ask("Сколько символов оставить в конце: ").toIntOption.getOrElse(0))
      case _ => ReplacementSpec(replKind) // remove; неверный тип найдёт проверка
    }

    val priority = ask("Приоритет (число): ").toIntOption
      .getOrElse(throw new ConfigException("Приоритет не является числом"))
    RuleDef(id, description, MatcherSpec(matcherKind, matcherValue), replacement, priority, enabled = true)
  }

  private def createPolicy(): Unit = {
    val name = ask("Имя новой политики: ")
    checkName(name)
    if (config.policies.contains(name)) throw new ConfigException("Политика с таким именем уже есть")
    val description = ask("Описание: ")
    val rules = mutable.ListBuffer[RuleDef]()
    while (confirm("Добавить правило?")) rules += askRule()
    commit(name, Policy(description, Nil, rules.toList), "Создание")
  }

  private def editPolicy(args: List[String]): Unit = {
    val name = argument(args, "имя политики")
    var current = policyOrFail(name)
    var done = false
    while (!done) {
      println("1 — добавить/заменить правило, 2 — удалить правило, 3 — вкл/выкл правило, 4 — описание, 0 — сохранить")
      ask("> ") match {
        case "1" =>
          val rule = askRule()
          current = current.copy(rules = current.rules.filterNot(_.id == rule.id) :+ rule)
        case "2" =>
          val id = ask("id правила: ")
          current = current.copy(rules = current.rules.filterNot(_.id == id))
        case "3" =>
          val id = ask("id правила: ")
          current = current.copy(rules = current.rules.map(r => if (r.id == id) r.copy(enabled = !r.enabled) else r))
        case "4" => current = current.copy(description = ask("Описание: "))
        case "0" => done = true
        case _ => println("Неизвестный пункт")
      }
    }
    commit(name, current, "Изменение")
  }

  private def deletePolicy(args: List[String]): Unit = {
    val name = argument(args, "имя политики")
    policyOrFail(name)
    if (name == config.defaultPolicy) throw new ConfigException("Нельзя удалить политику по умолчанию")
    val users = config.policies.collect { case (n, p) if p.includes.contains(name) => n }
    if (users.nonEmpty) throw new ConfigException(s"Политика входит в состав: ${users.mkString(", ")}")
    if (confirm(s"Удалить политику '$name'?")) {
      val updated = config.copy(policies = config.policies.removed(name))
      ConfigStore.save(configPath, updated)
      config = updated
      previewed = None
      println("Политика удалена.")
      Log.info(s"Удаление политики '$name'")
    }
  }

  // Составная политика хранит ссылки на другие политики, правила не копируются
  private def composePolicy(args: List[String]): Unit = {
    val name = argument(args, "имя политики")
    checkName(name)
    val includes =
      if (args.tail.nonEmpty) args.tail
      else ask("Включить политики (через пробел, порядок важен): ").split("\\s+").filter(_.nonEmpty).toList
    val policy = config.policies.get(name) match {
      case Some(existing) => existing.copy(includes = includes) // свои правила сохраняются
      case None => Policy(ask("Описание: "), includes, Nil)
    }
    commit(name, policy, "Составление")
  }

  private def validatePolicy(args: List[String]): Unit = {
    val name = argument(args, "имя политики")
    policyOrFail(name)
    val errors = Validator.validatePolicy(name, config)
    if (errors.isEmpty) {
      println(s"Политика '$name' корректна.")
      Log.info(s"Проверка политики '$name': ошибок нет")
      reportOverrides(name)
    } else {
      errors.foreach(e => println("  - " + e))
      Log.warn(s"Проверка политики '$name': ошибок ${errors.size}")
    }
  }

  // ---------- Команды: выбор файлов, предпросмотр, обработка ----------

  private def select(args: List[String]): Unit = {
    selectionRecursive = args.contains("-r")
    selection = args.filterNot(_ == "-r").map(a => Paths.get(a))
    println(s"Выбрано путей: ${selection.size}")
    Log.info(s"Выбрано путей: ${selection.size}")
  }

  // Общая подготовка для preview и anonymize: проверка, раскрытие политики, поиск файлов, анализ
  private def prepare(args: List[String]): Prepared = {
    val policy = argument(args, "политику: preview <политика> [-r] [пути]")
    policyOrFail(policy)
    val errors = Validator.validatePolicy(policy, config)
    if (errors.nonEmpty) throw new ConfigException("Политика некорректна:\n  - " + errors.mkString("\n  - "))

    val resolved = PolicyResolver.resolve(policy, config.policies)
    reportOverrides(policy)
    val rules = resolved.rules.filter(_.enabled).map(r => Rule.from(r))

    val explicitPaths = args.tail.filterNot(_ == "-r").map(a => Paths.get(a))
    val (paths, recursive) =
      if (explicitPaths.nonEmpty) (explicitPaths, args.tail.contains("-r")) else (selection, selectionRecursive)
    if (paths.isEmpty) throw new ConfigException("Не выбраны файлы или каталоги (команда select или аргументы)")

    Log.info("Начало анализа")
    val scan = Scanner.scan(paths, recursive, config)
    val outcomes = Processor.analyzeAll(scan, rules)
    val matches = outcomes.flatMap(_.analysis).map(_.accepted.size).sum
    Log.info(s"Анализ завершён: файлов ${outcomes.size}, совпадений $matches")
    Prepared(policy, resolved, rules, scan, outcomes, paths.mkString(", "),
      s"$policy|${paths.mkString(",")}|$recursive")
  }

  private def label(r: RuleDef): String = if (r.description.nonEmpty) r.description else r.id

  private def printSummary(p: Prepared): Unit = {
    val analyzed = p.outcomes.flatMap(_.analysis)
    val failed = p.outcomes.filter(_.error.isDefined)
    println(s"\nПолитика: ${p.policy}")
    println(s"Источник: ${p.sources}")
    println(s"Файлов найдено: ${p.outcomes.size + p.scan.skipped.size}")
    println(s"Файлов будет обработано: ${analyzed.size}")
    println(s"Файлов пропущено: ${p.scan.skipped.size}")
    p.scan.skipped.foreach { case (name, why) => println(s"  пропущен $name: $why") }
    failed.foreach(o => println(s"  ошибка ${o.source.relative}: ${o.error.getOrElse("")}"))

    val counts = analyzed.flatMap(_.accepted).groupBy(_.rule.defn.id).map { case (id, list) => id -> list.size }
    println("\nОбнаружено:")
    p.rules.foreach(r => println(s"  ${label(r.defn)}: ${counts.getOrElse(r.defn.id, 0)}"))
    println(s"  Конфликты правил: ${analyzed.map(_.disputed.size).sum}")
  }

  // Подробности по выбранному файлу: строка, исходное значение, замена
  private def browseDetails(p: Prepared): Unit = {
    val book = new PseudonymBook
    val files = p.outcomes.flatMap { o =>
      o.analysis.toList.map(a => (o.source.relative.toString, a, Engine.render(a, false, book)._2))
    }
    if (files.nonEmpty) {
      println()
      files.zipWithIndex.foreach { case ((name, a, changes), i) =>
        println(s"  ${i + 1}. $name (замен: ${changes.size}, конфликтов: ${a.disputed.size})")
      }
      // Один вопрос без повтора: можно ввести несколько номеров через пробел (например: 1 3) или "all".
      val line = ask("Номер файла для подробностей (Enter — продолжить): ")
      val chosen =
        if (line.equalsIgnoreCase("all")) files.indices.toList
        else line.split("[\\s,]+").toList.flatMap(t => t.toIntOption).filter(n => n >= 1 && n <= files.size).map(n => n - 1)
      chosen.foreach { i =>
        val (name, a, changes) = files(i)
        println(s"\nФайл: $name")
        changes.foreach(c => println(s"Строка ${c.line}:\n${c.before}\n→ ${c.after}\n"))
        a.disputed.foreach { c =>
          println(s"Строка ${Engine.lineOf(a.text, c.hit.start)}: КОНФЛИКТ (правило ${c.rule.defn.id}), участок не изменяется без подтверждения\n")
        }
      }
    }
  }

  private def preview(args: List[String]): Unit = {
    val p = prepare(args)
    printSummary(p)
    browseDetails(p)
    previewed = Some(p.key)
  }

  // anonymize работает и без предварительного preview, но тогда выводится предупреждение
  private def anonymize(args: List[String]): Unit = {
    val p = prepare(args)
    val analyzed = p.outcomes.flatMap(_.analysis)
    val matches = analyzed.map(_.accepted.size).sum
    val conflicts = analyzed.map(_.disputed.size).sum

    p.scan.skipped.foreach { case (name, why) => println(s"  пропущен $name: $why") }
    p.outcomes.filter(_.error.isDefined).foreach { o =>
      println(s"  ошибка ${o.source.relative}: ${o.error.getOrElse("")}")
    }
    println(s"Файлов к обработке: ${analyzed.size}, найдено совпадений: $matches, конфликтов: $conflicts")

    if (!previewed.contains(p.key)) {
      println("ВНИМАНИЕ: предпросмотр (preview) для этой политики и этих файлов не выполнялся.")
      println("Вы не увидите, что именно будет заменено. Рекомендуется сначала выполнить: preview " + p.policy)
      Log.warn("Запуск anonymize без предварительного preview")
    }

    if (analyzed.isEmpty) {
      println("Нет файлов для обработки.")
    } else if (!confirm(s"Создать копии в '${config.outputDirectory}'?")) {
      println("Отменено.")
    } else {
      val withDisputed = conflicts > 0 && confirm(s"Конфликтов правил: $conflicts. Заменить спорные участки?")
      Log.info("Начало обработки")
      val report = Processor.writeCopies(p.policy, p.resolved, p.scan, p.outcomes, config, withDisputed)
      Log.info(s"Обработка завершена: файлов ${report.processed}, ошибок ${report.errors.size}")
      lastReport = Some(report)
      println("\n" + report.toText)
      saveReport(report)
    }
  }

  // ---------- Отчёт и перезагрузка ----------

  private def saveReport(r: Report): Unit = {
    val stamp = r.started.format(DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss"))
    val file = Workspace.resolve(config.reportDirectory).resolve(s"report-$stamp.txt")
    try {
      Files.createDirectories(file.getParent)
      Files.writeString(file, r.toText)
      println("Отчёт сохранён: " + file)
    } catch {
      case _: java.io.IOException =>
        println("Не удалось сохранить отчёт в файл.")
        Log.error("Не удалось сохранить отчёт")
    }
  }

  private def showReport(): Unit =
    lastReport match {
      case Some(r) => println(r.toText)
      case None => println("Отчёта пока нет: сначала выполните anonymize.")
    }

  // При ошибке остаётся последняя корректная конфигурация
  private def reload(): Unit = {
    Log.info("Перезагрузка конфигурации")
    Try(ConfigStore.loadValidated(configPath)) match {
      case Success(newConfig) =>
        config = newConfig
        previewed = None
        Log.init(config.logFile)
        println("Конфигурация перезагружена.")
        Log.info("Конфигурация перезагружена")
      case Failure(e: ConfigException) =>
        println("Не удалось перезагрузить, используется предыдущая версия:\n" + e.getMessage)
        Log.warn("Перезагрузка не удалась, используется предыдущая конфигурация")
      case Failure(e) => throw e
    }
  }
}
