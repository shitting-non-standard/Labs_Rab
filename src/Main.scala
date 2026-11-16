//> using scala 3.10.0
//> using dep tools.jackson.core:jackson-databind:3.2.1

// ===== Менеджер политик анонимизации: консольный интерфейс =====
// Запуск: scala-cli run . -- anonymizer.json  или  sbt run (см. build.sbt)
//
// Слои программы:
//   Commands, Menu       — разбор ввода в ADT-команды (Command, EditAction)
//   Policies, Processing — предметная область: чистые функции, возвращают Either[AppError, ...]
//   Messages             — превращение данных и ошибок в текст
//   Main (этот файл)     — единственное место, где есть ввод с клавиатуры и вывод на экран

import java.nio.file.{Files, InvalidPathException, Path, Paths}
import java.time.format.DateTimeFormatter
import scala.annotation.tailrec
import scala.util.control.NonFatal

object Main {
  def main(args: Array[String]): Unit = {
    val loaded: Either[AppError, (Path, AppConfig)] =
      Safe(Paths.get(args.headOption.getOrElse("anonymizer.json")).toAbsolutePath) {
        case _: InvalidPathException => InvalidPath
      }.flatMap { path =>
        Workspace.baseDir = Option(path.getParent).getOrElse(path) // пути из конфигурации считаем от её папки
        ConfigStore.loadValidated(path).map(config => (path, config))
      }

    loaded match {
      case Left(error) =>
        println("Ошибка конфигурации:\n" + Messages.error(error))
        sys.exit(1)
      case Right((path, config)) =>
        Log.init(config.logFile).left.foreach(e => println("Предупреждение: " + Messages.error(e)))
        Log.info("Запуск приложения")
        Log.info("Конфигурация загружена и проверена")
        new App(path).run(config)
        Log.info("Завершение работы приложения")
    }
  }
}

// ---------- Ввод с клавиатуры ----------
object Terminal {
  // None — ввод закрыт (конец файла)
  def readLine(prompt: String): Option[String] = Option(scala.io.StdIn.readLine(prompt))

  // Ответ на вопрос. Пустая строка здесь — это именно пустой ответ пользователя.
  def ask(prompt: String): String = readLine(prompt).map(_.trim).getOrElse("")

  // Ответ, который можно не давать: пустой ответ и закрытый ввод — None
  def askOptional(prompt: String): Option[String] = readLine(prompt).map(_.trim).filter(_.nonEmpty)

  def askList(prompt: String): List[String] = ask(prompt).split("\\s+").toList.filter(_.nonEmpty)

  // Переспрашивает, пока не получит понятный ответ. Закрытый ввод считается отказом.
  @tailrec
  def confirm(question: String): Boolean =
    readLine(question + " (y/n): ") match {
      case None =>
        println("Ввод закрыт, действие отменено.")
        false
      case Some(line) =>
        line.trim.toLowerCase match {
          case "y" | "yes" | "д" | "да" => true
          case "n" | "no" | "н" | "нет" => false
          case _ =>
            println("Введите y или n.")
            confirm(question)
        }
    }
}

// ---------- Состояние программы (неизменяемое) ----------
case class AppState(
    config: AppConfig,
    selection: Selection,
    lastReport: Option[Report],
    previewed: Option[String], // для каких политики и файлов уже выполнялся preview
    running: Boolean)

// Каждый обработчик команды получает состояние и возвращает новое состояние.
class App(configPath: Path) {

  def run(config: AppConfig): Unit = {
    println("Менеджер политик анонимизации. Команда help — список команд.")
    loop(AppState(config, Selection(Nil, recursive = false), None, None, running = true))
    ()
  }

  // ---------- Главный цикл ----------

  @tailrec
  private def loop(state: AppState): AppState =
    if (!state.running) state
    else
      Terminal.readLine("> ") match {
        case None => state.copy(running = false) // конец ввода
        case Some(line) => loop(step(state, line))
      }

  // Один шаг: строка -> команда (ADT) -> новое состояние
  private def step(state: AppState, line: String): AppState =
    Commands.parseLine(line) match {
      case None => state
      case Some(Left(error)) =>
        showError(error)
        state
      case Some(Right(command)) =>
        try {
          handle(state, command)
        } catch {
          // непредвиденная ошибка в программе: не роняем приложение
          case NonFatal(e) =>
            println("Внутренняя ошибка приложения.")
            Log.error("Внутренняя ошибка: " + e.getClass.getSimpleName)
            state
        }
    }

  // Интерпретатор команд: по варианту ADT выбирается действие
  private def handle(state: AppState, command: Command): AppState = command match {
    case ShowHelp =>
      Commands.registry.print()
      state
    case ListPolicies =>
      listPolicies(state.config)
      state
    case ShowPolicy(name) =>
      showPolicy(state.config, name)
      state
    case CreatePolicy => createPolicy(state)
    case EditPolicy(name) => editPolicy(state, name)
    case DeletePolicy(name) => deletePolicy(state, name)
    case ComposePolicy(name, includes) => composePolicy(state, name, includes)
    case ValidatePolicy(name) =>
      validatePolicy(state.config, name)
      state
    case SelectPaths(paths, recursive) =>
      println(s"Выбрано путей: ${paths.size}")
      Log.info(s"Выбрано путей: ${paths.size}")
      state.copy(selection = Selection(paths, recursive))
    case Preview(request) => preview(state, request)
    case Anonymize(request) => anonymize(state, request)
    case ShowReport =>
      state.lastReport.fold(println("Отчёта пока нет: сначала выполните anonymize."))(r => println(Messages.report(r)))
      state
    case ReloadConfig => reload(state)
    case Exit => state.copy(running = false)
  }

  // ---------- Вывод ошибок ----------

  private def showError(error: AppError): Unit = {
    val text = Messages.error(error)
    println("Ошибка: " + text)
    error match {
      case _: InputError => () // ввод пользователя в журнал не пишем
      case _ => Log.warn(text)
    }
  }

  private def showProblems(problems: List[AppError]): Unit =
    problems.foreach(p => println("  - " + Messages.error(p)))

  private def reportOverrides(cfg: AppConfig, name: String): Unit =
    PolicyResolver.resolve(name, cfg.policies).foreach { resolved =>
      resolved.overrides.foreach { o =>
        val text = Messages.overrideText(o)
        println("Переопределение: " + text)
        Log.info("Переопределение: " + text) // только имена политик и id правил
      }
    }

  // ---------- Просмотр политик ----------

  private def listPolicies(cfg: AppConfig): Unit = {
    cfg.policies.foreach { case (name, p) =>
      val inc = if (p.includes.nonEmpty) s" (включает: ${p.includes.mkString(", ")})" else ""
      val star = if (name == cfg.defaultPolicy) " *" else ""
      println(s"  $name$star${p.description.fold("")(d => " — " + d)}$inc")
    }
    println("(* — политика по умолчанию)")
  }

  private def showPolicy(cfg: AppConfig, name: String): Unit =
    PolicyOps.find(cfg, name).fold(e => showError(e), { p =>
      println(s"Политика: $name")
      p.description.foreach(d => println("Описание: " + d))
      if (p.includes.nonEmpty) println("Включает: " + p.includes.mkString(", "))
      println("Собственные правила:")
      p.rules.foreach(r => println("  " + Messages.describeRule(r)))
      PolicyResolver.resolve(name, cfg.policies).fold(e => showError(e), { resolved =>
        println("Итоговый набор правил: " + resolved.rules.map(_.id).mkString(", "))
        resolved.overrides.foreach(o => println("  ! " + Messages.overrideText(o)))
      })
    })

  private def validatePolicy(cfg: AppConfig, name: String): Unit =
    PolicyOps.find(cfg, name).fold(e => showError(e), { _ =>
      Validator.validatePolicy(name, cfg) match {
        case Nil =>
          println(s"Политика '$name' корректна.")
          Log.info(s"Проверка политики '$name': ошибок нет")
          reportOverrides(cfg, name)
        case problems =>
          showProblems(problems)
          Log.warn(s"Проверка политики '$name': ошибок ${problems.size}")
      }
    })

  // ---------- Изменение политик ----------

  // Сохраняет политику, только если она корректна и конфигурация записана. Иначе состояние не меняется.
  private def commit(state: AppState, name: String, policy: Policy, action: String): AppState = {
    val result =
      for {
        updated <- PolicyOps.put(state.config, name, policy)
        _ <- ConfigStore.save(configPath, updated)
      } yield updated

    result match {
      case Right(updated) =>
        println(s"Политика '$name' сохранена.")
        Log.info(s"$action политики '$name'")
        reportOverrides(updated, name)
        state.copy(config = updated, previewed = None)
      case Left(PolicyRejected(_, problems)) =>
        println("Политика не сохранена, найдены ошибки:")
        showProblems(problems)
        Log.warn(s"Политика '$name' не прошла проверку, ошибок: ${problems.size}")
        state
      case Left(error) =>
        showError(error)
        state
    }
  }

  private def matcherPrompt(kind: MatcherKind): String = kind match {
    case MatcherKind.Dictionary => "Путь к файлу словаря: "
    case MatcherKind.Builtin => s"Тип (${BuiltinType.keys}): "
    case MatcherKind.Exact | MatcherKind.Regex => "Значение или выражение: "
  }

  // Количество символов: не указано — None, не число — ошибка
  private def askCount(where: String): Either[AppError, Option[Int]] =
    Eithers.sequenceOption(
      Terminal.askOptional(s"Сколько символов оставить в $where: ").map(text => text.toIntOption.toRight(InvalidCountInput)))

  // Какие дополнительные вопросы задать, определяется видом замены (ADT), а не сравнением строк
  private def askDetails(kind: ReplacementKind): Either[AppError, ReplacementDetails] = kind match {
    case ReplacementKind.Constant => Right(ReplacementDetails(value = Terminal.askOptional("Строка замены: ")))
    case ReplacementKind.Pseudonym =>
      Right(ReplacementDetails(template = Terminal.askOptional("Шаблон, например user{number}@example.com: ")))
    case ReplacementKind.Values =>
      Right(ReplacementDetails(values =
        Terminal.askOptional("Значения через ';': ").map(_.split(";").map(_.trim).filter(_.nonEmpty).toList).getOrElse(Nil)))
    case ReplacementKind.Mask =>
      for {
        start <- askCount("начале")
        end <- askCount("конце")
      } yield ReplacementDetails(keepStart = start, keepEnd = end)
    case ReplacementKind.Remove => Right(ReplacementDetails())
  }

  // Диалог: ответы пользователя на вопросы о правиле. Вид поиска и замены разбираются сразу:
  // при неверном виде диалог прекращается, остальные вопросы не задаются. Ввод не логируется.
  private def askDraft(): Either[AppError, RuleDraft] = {
    val description = Terminal.askOptional("Описание: ")
    for {
      matcherKind <- MatcherKind.parse(Terminal.ask(s"Способ поиска (${MatcherKind.keys}): "))
      matcherValue = Terminal.ask(matcherPrompt(matcherKind))
      replacementKind <- ReplacementKind.parse(Terminal.ask(s"Способ замены (${ReplacementKind.keys}): "))
      details <- askDetails(replacementKind)
      priority = Terminal.ask("Приоритет (число): ").toIntOption
    } yield RuleDraft(description, matcherKind, matcherValue, replacementKind, details, priority)
  }

  // Диалог + предметная логика: из ответов строится правило с автоматическим идентификатором
  private def askRule(cfg: AppConfig, extra: List[RuleDef]): Either[AppError, RuleDef] =
    askDraft().flatMap(draft => RuleBuilder.build(draft, PolicyOps.takenIds(cfg, extra)))

  @tailrec
  private def collectRules(cfg: AppConfig, rules: List[RuleDef]): List[RuleDef] =
    if (!Terminal.confirm("Добавить правило?")) rules
    else
      askRule(cfg, rules) match {
        case Right(rule) =>
          println(s"Правило создано, идентификатор: ${rule.id}")
          collectRules(cfg, rules :+ rule)
        case Left(error) =>
          showError(error)
          collectRules(cfg, rules)
      }

  private def createPolicy(state: AppState): AppState =
    PolicyOps.validateName(Terminal.ask("Имя новой политики: ")).flatMap(n => PolicyOps.ensureNew(state.config, n)) match {
      case Left(error) =>
        showError(error)
        state
      case Right(name) =>
        val description = Terminal.askOptional("Описание: ")
        val rules = collectRules(state.config, Nil)
        commit(state, name, Policy(description, Nil, rules), "Создание")
    }

  // Показывает правила списком и просит выбрать номер. Нет выбора — None.
  private def pickRule(policy: Policy): Option[RuleDef] =
    if (policy.rules.isEmpty) {
      println("В политике нет правил.")
      None
    } else {
      policy.rules.zipWithIndex.foreach { case (r, i) => println(s"  ${i + 1}. ${Messages.describeRule(r)}") }
      val chosen = Terminal.ask("Номер правила: ").toIntOption.flatMap(n => policy.rules.lift(n - 1))
      if (chosen.isEmpty) println("Неверный номер.")
      chosen
    }

  // Редактирование: меню даёт EditAction, дальше работаем только с ним
  @tailrec
  private def editLoop(cfg: AppConfig, policy: Policy): Policy = {
    Commands.editMenu.print()
    Terminal.readLine("> ") match {
      case None => policy // ввод закрыт
      case Some(line) =>
        Commands.editMenu.parse(Commands.tokenize(line)) match {
          case Left(error) =>
            showError(error)
            editLoop(cfg, policy)
          case Right(FinishEditing) => policy
          case Right(AddRule) =>
            askRule(cfg, policy.rules) match {
              case Right(rule) =>
                println(s"Правило создано, идентификатор: ${rule.id}")
                editLoop(cfg, PolicyOps.addRule(policy, rule))
              case Left(error) =>
                showError(error)
                editLoop(cfg, policy)
            }
          case Right(RemoveRule) =>
            editLoop(cfg, pickRule(policy).fold(policy)(r => PolicyOps.removeRule(policy, r.id)))
          case Right(ToggleRule) =>
            editLoop(cfg, pickRule(policy).fold(policy)(r => PolicyOps.toggleRule(policy, r.id)))
          case Right(ChangeDescription) =>
            editLoop(cfg, PolicyOps.describe(policy, Terminal.askOptional("Описание: ")))
        }
    }
  }

  private def editPolicy(state: AppState, name: String): AppState =
    PolicyOps.find(state.config, name) match {
      case Left(error) =>
        showError(error)
        state
      case Right(policy) => commit(state, name, editLoop(state.config, policy), "Изменение")
    }

  private def deletePolicy(state: AppState, name: String): AppState =
    PolicyOps.remove(state.config, name) match {
      case Left(error) =>
        showError(error)
        state
      case Right(updated) =>
        if (!Terminal.confirm(s"Удалить политику '$name'?")) state
        else
          ConfigStore.save(configPath, updated).fold(
            error => {
              showError(error)
              state
            },
            _ => {
              println("Политика удалена.")
              Log.info(s"Удаление политики '$name'")
              state.copy(config = updated, previewed = None)
            })
    }

  // Составная политика хранит ссылки на другие политики, правила не копируются
  private def composePolicy(state: AppState, name: String, includes: Option[List[String]]): AppState =
    PolicyOps.validateName(name) match {
      case Left(error) =>
        showError(error)
        state
      case Right(valid) =>
        val chosen = includes.getOrElse(Terminal.askList("Включить политики (через пробел, порядок важен): "))
        val policy = state.config.policies.get(valid)
          .map(existing => existing.copy(includes = chosen)) // свои правила сохраняются
          .getOrElse(Policy(Terminal.askOptional("Описание: "), chosen, Nil))
        commit(state, valid, policy, "Составление")
    }

  // ---------- Предпросмотр и обработка ----------

  private def logAnalysis(p: Prepared): Unit = {
    Log.info("Начало анализа")
    p.outcomes.foreach(o => o.analysis.left.foreach(_ => Log.error(s"Ошибка чтения файла '${o.source.relative}'")))
    val matches = p.outcomes.flatMap(o => o.analysis.toOption).map(_.analysis.accepted.size).sum
    Log.info(s"Анализ завершён: файлов ${p.outcomes.size}, совпадений $matches")
  }

  private def printSummary(p: Prepared): Unit = {
    val analyzed = p.outcomes.flatMap(o => o.analysis.toOption)
    val failed = p.outcomes.flatMap(o => o.analysis.left.toOption.map(e => (o.source.relative, e)))
    println(s"\nПолитика: ${p.policy}")
    println(s"Источник: ${p.paths.mkString(", ")}")
    println(s"Файлов найдено: ${p.outcomes.size + p.scan.skipped.size}")
    println(s"Файлов будет обработано: ${analyzed.size}")
    println(s"Файлов пропущено: ${p.scan.skipped.size}")
    p.scan.skipped.foreach(s => println("  " + Messages.skipped(s)))
    failed.foreach { case (file, e) => println(s"  ошибка $file: ${Messages.error(e)}") }

    val counts = analyzed.flatMap(_.analysis.accepted).groupBy(_.rule.defn.id).map { case (id, list) => id -> list.size }
    println("\nОбнаружено:")
    p.rules.foreach(r => println(s"  ${Messages.label(r.defn)}: ${counts.getOrElse(r.defn.id, 0)}"))
    println(s"  Конфликты правил: ${analyzed.map(_.analysis.disputed.size).sum}")
  }

  // Подробности по выбранным файлам: строка, исходное значение, замена.
  // Один вопрос без повтора: номера через пробел (1 3) или all.
  private def browseDetails(p: Prepared): Unit = {
    val book = new PseudonymBook
    val files = p.outcomes.flatMap { o =>
      o.analysis.toOption.map(a => (o.source.relative.toString, a.analysis, Engine.render(a.analysis, false, book)._2))
    }
    if (files.nonEmpty) {
      println()
      files.zipWithIndex.foreach { case ((name, a, changes), i) =>
        println(s"  ${i + 1}. $name (замен: ${changes.size}, конфликтов: ${a.disputed.size})")
      }
      val line = Terminal.ask("Номера файлов для подробностей, all — все (Enter — закончить просмотр): ")
      val chosen =
        if (line.equalsIgnoreCase("all")) files.indices.toList
        else line.split("[\\s,]+").toList.flatMap(t => t.toIntOption).filter(n => n >= 1 && n <= files.size).map(n => n - 1)
      chosen.flatMap(i => files.lift(i)).foreach { case (name, a, changes) =>
        println(s"\nФайл: $name")
        changes.foreach(c => println(s"Строка ${c.line}:\n${c.before}\n→ ${c.after}\n"))
        a.disputed.foreach { c =>
          println(s"Строка ${Engine.lineOf(a.text, c.hit.start)}: КОНФЛИКТ (правило ${c.rule.defn.id}), участок не изменяется без подтверждения\n")
        }
      }
    }
  }

  private def preview(state: AppState, request: ScanRequest): AppState =
    Anonymization.prepare(state.config, request, state.selection) match {
      case Left(error) =>
        showError(error)
        state
      case Right(p) =>
        reportOverrides(state.config, p.policy)
        logAnalysis(p)
        printSummary(p)
        browseDetails(p)
        println("\nПредпросмотр завершён, файлы не изменены. Чтобы создать копии: anonymize " + p.policy + " ...")
        state.copy(previewed = Some(p.key))
    }

  // anonymize работает и без предварительного preview, но тогда выводится предупреждение
  private def anonymize(state: AppState, request: ScanRequest): AppState =
    Anonymization.prepare(state.config, request, state.selection) match {
      case Left(error) =>
        showError(error)
        state
      case Right(p) =>
        reportOverrides(state.config, p.policy)
        logAnalysis(p)
        val analyzed = p.outcomes.flatMap(o => o.analysis.toOption)
        val matches = analyzed.map(_.analysis.accepted.size).sum
        val conflicts = analyzed.map(_.analysis.disputed.size).sum

        p.scan.skipped.foreach(s => println("  " + Messages.skipped(s)))
        p.outcomes.foreach(o => o.analysis.left.foreach(e => println(s"  ошибка ${o.source.relative}: ${Messages.error(e)}")))
        println(s"Файлов к обработке: ${analyzed.size}, найдено совпадений: $matches, конфликтов: $conflicts")

        if (!state.previewed.contains(p.key)) {
          println("ВНИМАНИЕ: предпросмотр (preview) для этой политики и этих файлов не выполнялся.")
          println("Вы не увидите, что именно будет заменено. Рекомендуется сначала выполнить: preview " + p.policy)
          Log.warn("Запуск anonymize без предварительного preview")
        }

        if (analyzed.isEmpty) {
          println("Нет файлов для обработки.")
          state
        } else if (!Terminal.confirm(s"Создать копии в '${state.config.outputDirectory}'?")) {
          println("Отменено.")
          state
        } else {
          val withDisputed = conflicts > 0 && Terminal.confirm(s"Конфликтов правил: $conflicts. Заменить спорные участки?")
          val overwrite = askOverwrite(state.config, Processor.existingTargets(p.outcomes, state.config))
          Log.info("Начало обработки")
          val report = Processor.writeCopies(p.policy, p.resolved, p.scan, p.outcomes, state.config, withDisputed, overwrite)
          report.errors.foreach(f => Log.error(s"Ошибка обработки файла '${f.file}'"))
          Log.info(s"Обработка завершена: файлов ${report.processed}, ошибок ${report.errors.size}")
          println("\n" + Messages.report(report))
          saveReport(state.config, report)
          state.copy(lastReport = Some(report))
        }
    }

  // Результаты уже существуют: спрашиваем, перезаписывать ли. Если в конфигурации включено
  // overwriteExistingFiles, перезапись выполняется без вопроса.
  private def askOverwrite(cfg: AppConfig, existing: List[String]): OverwritePolicy =
    if (existing.isEmpty || cfg.overwriteExistingFiles) {
      Overwrite
    } else {
      println(s"Результат уже существует для файлов (${existing.size}):")
      existing.foreach(name => println("  " + name))
      if (Terminal.confirm("Перезаписать существующие файлы?")) Overwrite
      else {
        println("Существующие файлы не будут перезаписаны.")
        KeepExisting
      }
    }

  // ---------- Отчёт и перезагрузка ----------

  private def saveReport(cfg: AppConfig, r: Report): Unit = {
    val saved = Safe {
      val stamp = r.started.format(DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss"))
      val file = Workspace.resolve(cfg.reportDirectory).resolve(s"report-$stamp.txt")
      Option(file.getParent).foreach(d => Files.createDirectories(d))
      Files.writeString(file, Messages.report(r))
      file
    } {
      case _: java.io.IOException => ReportSaveFailed
    }
    saved.fold(e => showError(e), file => println("Отчёт сохранён: " + file))
  }

  // При ошибке остаётся последняя корректная конфигурация
  private def reload(state: AppState): AppState = {
    Log.info("Перезагрузка конфигурации")
    ConfigStore.loadValidated(configPath) match {
      case Right(newConfig) =>
        Log.init(newConfig.logFile).left.foreach(e => println("Предупреждение: " + Messages.error(e)))
        println("Конфигурация перезагружена.")
        Log.info("Конфигурация перезагружена")
        state.copy(config = newConfig, previewed = None)
      case Left(error) =>
        println("Не удалось перезагрузить, используется предыдущая версия:\n" + Messages.error(error))
        Log.warn("Перезагрузка не удалась, используется предыдущая конфигурация")
        state
    }
  }
}
