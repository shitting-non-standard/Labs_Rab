// ===== Обработка текста и файлов =====
// Функции возвращают данные или Either[AppError, ...]. Вывода на экран и чтения ввода здесь нет.

import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}
import java.nio.file.{Files, Path}
import java.time.LocalDateTime
import scala.jdk.CollectionConverters.*

// Совпадение + правило, которое его нашло
case class Candidate(rule: Rule, hit: Hit)

// Одна выполненная замена (для предпросмотра и отчёта)
case class Change(line: Int, ruleId: String, before: String, after: String)

// Результат анализа текста:
//   accepted — совпадения, которые будут заменены
//   disputed — спорные участки (правила равны по силе), без подтверждения не меняются
case class TextAnalysis(text: String, accepted: List[Candidate], disputed: List[Candidate])

object Engine {

  // Состояние жадного выбора совпадений (неизменяемое)
  private case class Pick(chosen: Vector[Candidate], disputed: Set[Candidate])

  // Шаг 1. Ищем ВСЕ совпадения в исходном тексте, до каких-либо замен.
  // Шаг 2. Выбираем победителей среди пересекающихся совпадений.
  def analyze(text: String, rules: List[Rule]): TextAnalysis = {
    val found = rules.flatMap(r => r.matcher.find(text).map(h => Candidate(r, h)))

    // Порядок: больший priority, затем более длинное совпадение, затем позиция и id (для повторяемости)
    val ordered = found.sortBy { c =>
      (-c.rule.defn.priority, -(c.hit.end - c.hit.start), c.hit.start, c.rule.defn.id)
    }

    val picked = ordered.foldLeft(Pick(Vector.empty, Set.empty)) { (pick, c) =>
      val clashes = pick.chosen.filter(o => overlaps(c, o))
      if (clashes.isEmpty) {
        pick.copy(chosen = pick.chosen :+ c)
      } else {
        // Проигравший отбрасывается. Но если у соперника тот же приоритет и та же длина,
        // победителя определить нельзя — участок помечается как конфликт.
        pick.copy(disputed = pick.disputed ++ clashes.filter(o => samePower(o, c)))
      }
    }

    TextAnalysis(
      text,
      picked.chosen.filterNot(c => picked.disputed.contains(c)).sortBy(_.hit.start).toList,
      picked.chosen.filter(c => picked.disputed.contains(c)).sortBy(_.hit.start).toList)
  }

  private def overlaps(a: Candidate, b: Candidate): Boolean =
    a.hit.start < b.hit.end && b.hit.start < a.hit.end

  private def samePower(a: Candidate, b: Candidate): Boolean =
    a.rule.defn.priority == b.rule.defn.priority &&
      (a.hit.end - a.hit.start) == (b.hit.end - b.hit.start)

  // Состояние при сборке нового текста
  private case class Built(pos: Int, line: Int, parts: Vector[String], changes: Vector[Change])

  // Шаг 3. Выполняем замены. withDisputed = true — заменить и спорные участки.
  def render(a: TextAnalysis, withDisputed: Boolean, book: PseudonymBook): (String, List[Change]) = {
    val used = (if (withDisputed) a.accepted ++ a.disputed else a.accepted).sortBy(_.hit.start)
    val built = used.foldLeft(Built(0, 1, Vector.empty, Vector.empty)) { (state, c) =>
      val between = a.text.substring(state.pos, c.hit.start)
      val line = state.line + between.count(_ == '\n')
      val after = c.rule.replacement(c.hit, c.rule.defn.id, book)
      Built(
        pos = c.hit.end,
        line = line + c.hit.text.count(_ == '\n'),
        parts = state.parts :+ between :+ after,
        changes = state.changes :+ Change(line, c.rule.defn.id, c.hit.text, after))
    }
    (built.parts.mkString + a.text.substring(built.pos), built.changes.toList)
  }

  def lineOf(text: String, pos: Int): Int = text.substring(0, pos).count(_ == '\n') + 1
}

// ===== Поиск файлов =====

// path — полный путь, relative — путь внутри выбранного каталога (для структуры результата)
case class SourceFile(path: Path, relative: Path)

// Почему файл пропущен
sealed trait SkipReason
case object PathMissing extends SkipReason
case object UnsupportedExtension extends SkipReason

case class Skipped(name: String, reason: SkipReason)

case class Scan(files: List[SourceFile], skipped: List[Skipped])

object Scanner {
  // Найденная запись: существующий файл или несуществующий путь
  private sealed trait Entry
  private case class Present(file: Path, relative: Path) extends Entry
  private case class Absent(name: String) extends Entry

  def scan(paths: List[Path], recursive: Boolean, cfg: AppConfig): Either[AppError, Scan] = {
    val outDir = Workspace.resolve(cfg.outputDirectory)
    val repDir = Workspace.resolve(cfg.reportDirectory)

    def folderOf(p: Path): Path = {
      val abs = p.toAbsolutePath.normalize
      if (Files.isDirectory(p)) abs else Option(abs.getParent).getOrElse(abs)
    }

    def expand(p: Path): Either[AppError, List[Entry]] =
      if (!Files.exists(p)) Right(List(Absent(p.toString)))
      else if (Files.isDirectory(p)) listFiles(p, recursive).map(files => files.map(f => Present(f, p.relativize(f))))
      else Right(List(Present(p, Option(p.getFileName).getOrElse(p))))

    // None — файл в папке результата или отчётов: свои результаты не обрабатываем
    def classify(entry: Entry): Option[Either[Skipped, SourceFile]] = entry match {
      case Absent(name) => Some(Left(Skipped(name, PathMissing)))
      case Present(file, relative) =>
        val abs = file.toAbsolutePath.normalize
        val name = Option(file.getFileName).map(_.toString.toLowerCase).getOrElse("")
        if (abs.startsWith(outDir) || abs.startsWith(repDir)) None
        else if (!cfg.supportedExtensions.exists(ext => name.endsWith(ext.toLowerCase)))
          Some(Left(Skipped(relative.toString, UnsupportedExtension)))
        else Some(Right(SourceFile(abs, relative)))
    }

    if (paths.exists(p => folderOf(p) == outDir)) {
      Left(OutputEqualsSource)
    } else {
      Eithers.traverse(paths)(p => expand(p)).map { entries =>
        val (skipped, files) = entries.flatten.flatMap(e => classify(e)).partitionMap(e => e)
        Scan(files.distinctBy(_.path).sortBy(_.path.toString), skipped)
      }
    }
  }

  private def listFiles(dir: Path, recursive: Boolean): Either[AppError, List[Path]] =
    Safe {
      val stream = if (recursive) Files.walk(dir) else Files.list(dir)
      try stream.iterator.asScala.filter(f => Files.isRegularFile(f)).toList
      finally stream.close()
    } {
      case _: java.io.IOException => FileIoError
    }
}

// ===== Чтение, анализ и запись файлов =====

// Время изменения и размер: по ним видно, что файл изменился во время обработки
case class FileStamp(modified: Long, size: Long)

case class FileAnalysis(analysis: TextAnalysis, stamp: FileStamp)

// Результат анализа одного файла: либо анализ, либо причина неудачи
case class FileOutcome(source: SourceFile, analysis: Either[AppError, FileAnalysis])

case class FileFailure(file: String, error: AppError)

case class Report(
    policy: String,
    included: List[String],
    overrides: List[RuleOverride],
    started: LocalDateTime,
    finished: LocalDateTime,
    processed: Int,
    skipped: Int,
    replacements: List[(String, Int)], // (id правила, количество замен)
    conflicts: Int,
    errors: List[FileFailure],
    kept: List[String],                // файлы, которые не перезаписаны по решению пользователя
    outputDir: String) {
  def complete: Boolean = errors.isEmpty
}

// Что делать с уже существующими результатами (решение принимает пользователь, не предметная область)
sealed trait OverwritePolicy
case object Overwrite extends OverwritePolicy
case object KeepExisting extends OverwritePolicy

// Итог записи одного файла
sealed trait WriteResult
case class Written(changes: List[Change]) extends WriteResult
case object KeptExisting extends WriteResult

object Processor {

  private def stampOf(p: Path): Either[AppError, FileStamp] =
    Safe(FileStamp(Files.getLastModifiedTime(p).toMillis, Files.size(p))) {
      case _: java.io.IOException => FileIoError
    }

  // Строгое чтение UTF-8: неверные байты дают ошибку, а не молча портятся
  private def readUtf8(path: Path): Either[AppError, String] =
    Safe {
      val decoder = StandardCharsets.UTF_8.newDecoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
      decoder.decode(ByteBuffer.wrap(Files.readAllBytes(path))).toString
    } {
      case _: CharacterCodingException => UnsupportedEncoding
      case _: java.io.IOException => FileIoError
    }

  // Анализ всех файлов. Ошибка одного файла не останавливает остальные.
  def analyzeAll(scan: Scan, rules: List[Rule]): List[FileOutcome] =
    scan.files.map { f =>
      FileOutcome(
        f,
        for {
          stamp <- stampOf(f.path)
          text <- readUtf8(f.path)
        } yield FileAnalysis(Engine.analyze(text, rules), stamp))
    }

  // Куда будет записана копия исходного файла
  private def targetOf(source: SourceFile, outDir: Path, cfg: AppConfig): Path =
    outDir.resolve(
      if (cfg.preserveDirectoryStructure) source.relative
      else Option(source.relative.getFileName).getOrElse(source.relative))

  // Имена исходных файлов, для которых результат уже существует. По этому списку интерфейс
  // решает, нужно ли спрашивать пользователя о перезаписи. Сама функция ничего не спрашивает.
  def existingTargets(outcomes: List[FileOutcome], cfg: AppConfig): List[String] = {
    val outDir = Workspace.resolve(cfg.outputDirectory)
    outcomes
      .filter(o => o.analysis.isRight)
      .filter(o => Files.exists(targetOf(o.source, outDir, cfg)))
      .map(o => o.source.relative.toString)
  }

  // Проверяет, что исходный файл не менялся, и записывает результат
  private def store(
      outcome: FileOutcome,
      file: FileAnalysis,
      target: Path,
      rendered: (String, List[Change])): Either[AppError, WriteResult] =
    for {
      current <- stampOf(outcome.source.path)
      _ <- Either.cond(current == file.stamp, (), SourceChanged)
      _ <- Safe {
        Option(target.getParent).foreach(d => Files.createDirectories(d))
        Files.write(target, rendered._1.getBytes(StandardCharsets.UTF_8))
        ()
      } {
        case _: java.io.IOException => FileIoError
      }
    } yield Written(rendered._2)

  // Обрабатывает один файл. Замена выполняется всегда (так нумерация псевдонимов не зависит от того,
  // перезаписан ли файл); существующий результат остаётся нетронутым, если пользователь не разрешил перезапись.
  private def writeOne(
      outcome: FileOutcome,
      outDir: Path,
      cfg: AppConfig,
      withDisputed: Boolean,
      overwrite: OverwritePolicy,
      book: PseudonymBook): Either[AppError, WriteResult] =
    for {
      file <- outcome.analysis
      rendered = Engine.render(file.analysis, withDisputed, book)
      target = targetOf(outcome.source, outDir, cfg)
      result <-
        if (Files.exists(target) && overwrite == KeepExisting) Right(KeptExisting: WriteResult)
        else store(outcome, file, target, rendered)
    } yield result

  // Создаёт анонимизированные копии. Исходные файлы не изменяются.
  def writeCopies(
      policy: String,
      resolved: ResolvedPolicy,
      scan: Scan,
      outcomes: List[FileOutcome],
      cfg: AppConfig,
      withDisputed: Boolean,
      overwrite: OverwritePolicy): Report = {

    val started = LocalDateTime.now()
    val outDir = Workspace.resolve(cfg.outputDirectory)
    val book = new PseudonymBook // один набор псевдонимов на всю операцию

    // List.map выполняется по порядку, поэтому нумерация псевдонимов повторяема
    val results = outcomes.map(o => (o, writeOne(o, outDir, cfg, withDisputed, overwrite, book)))

    val changes = results.flatMap { case (_, result) =>
      result.toOption.collect { case Written(done) => done }
    }.flatten
    val ruleIds = changes.map(_.ruleId).distinct
    val failures = results.flatMap { case (o, result) =>
      result.left.toOption.map(error => FileFailure(o.source.relative.toString, error))
    }
    val kept = results.collect { case (o, Right(KeptExisting)) => o.source.relative.toString }
    val conflicts = outcomes.flatMap(o => o.analysis.toOption).map(_.analysis.disputed.size).sum

    Report(
      policy = policy,
      included = resolved.policies,
      overrides = resolved.overrides,
      started = started,
      finished = LocalDateTime.now(),
      processed = results.count { case (_, result) => result.exists(r => r != KeptExisting) },
      skipped = scan.skipped.size,
      replacements = ruleIds.map(id => id -> changes.count(_.ruleId == id)),
      conflicts = conflicts,
      errors = failures,
      kept = kept,
      outputDir = outDir.toString)
  }
}

// ===== Подготовка операции (общая для preview и anonymize) =====

case class Prepared(
    policy: String,
    resolved: ResolvedPolicy,
    rules: List[Rule],
    scan: Scan,
    outcomes: List[FileOutcome],
    paths: List[Path],
    key: String) // политика + пути + режим: по ключу узнаём, выполнялся ли preview

object Anonymization {
  // Цепочка шагов: каждый может завершиться ошибкой, первая ошибка прекращает цепочку.
  def prepare(cfg: AppConfig, request: ScanRequest, selection: Selection): Either[AppError, Prepared] = {
    val (paths, recursive) =
      if (request.paths.nonEmpty) (request.paths, request.recursive)
      else (selection.paths, selection.recursive)
    for {
      _ <- PolicyOps.find(cfg, request.policy)
      _ <- Validator.ensureValid(request.policy, cfg)
      resolved <- PolicyResolver.resolve(request.policy, cfg.policies)
      rules <- Eithers.traverse(resolved.rules.filter(_.enabled))(r => Rule.from(r))
      _ <- Either.cond(paths.nonEmpty, (), NoPathsSelected)
      scan <- Scanner.scan(paths, recursive, cfg)
      outcomes = Processor.analyzeAll(scan, rules)
    } yield Prepared(request.policy, resolved, rules, scan, outcomes, paths,
      s"${request.policy}|${paths.mkString(",")}|$recursive")
  }
}
