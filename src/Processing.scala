// ===== Обработка текста и файлов =====

import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

// Совпадение + правило, которое его нашло
case class Candidate(rule: Rule, hit: Hit)

// Одна выполненная замена (для предпросмотра и отчёта)
case class Change(line: Int, ruleId: String, before: String, after: String)

// Результат анализа текста:
//   accepted — совпадения, которые будут заменены
//   disputed — спорные участки (правила равны по силе), без подтверждения не меняются
case class TextAnalysis(text: String, accepted: List[Candidate], disputed: List[Candidate])

object Engine {

  // Шаг 1. Ищем ВСЕ совпадения в исходном тексте, до каких-либо замен.
  // Шаг 2. Выбираем победителей среди пересекающихся совпадений.
  def analyze(text: String, rules: List[Rule]): TextAnalysis = {
    val found = rules.flatMap(r => r.matcher.find(text).map(h => Candidate(r, h)))

    // Порядок: больший priority, затем более длинное совпадение, затем позиция и id (для повторяемости)
    val ordered = found.sortBy { c =>
      (-c.rule.defn.priority, -(c.hit.end - c.hit.start), c.hit.start, c.rule.defn.id)
    }

    val chosen = mutable.ListBuffer[Candidate]()
    val disputed = mutable.Set[Candidate]()
    for (c <- ordered) {
      val clashes = chosen.filter(o => c.hit.start < o.hit.end && o.hit.start < c.hit.end)
      if (clashes.isEmpty) {
        chosen += c
      } else {
        // Проигравший отбрасывается. Но если у соперника тот же приоритет и та же длина,
        // победителя определить нельзя — участок помечается как конфликт.
        clashes.filter(o => samePower(o, c)).foreach(o => disputed += o)
      }
    }

    TextAnalysis(
      text,
      chosen.filterNot(c => disputed.contains(c)).sortBy(_.hit.start).toList,
      chosen.filter(c => disputed.contains(c)).sortBy(_.hit.start).toList)
  }

  private def samePower(a: Candidate, b: Candidate): Boolean =
    a.rule.defn.priority == b.rule.defn.priority &&
      (a.hit.end - a.hit.start) == (b.hit.end - b.hit.start)

  // Шаг 3. Выполняем замены. withDisputed = true — заменить и спорные участки.
  def render(a: TextAnalysis, withDisputed: Boolean, book: PseudonymBook): (String, List[Change]) = {
    val used = (if (withDisputed) a.accepted ++ a.disputed else a.accepted).sortBy(_.hit.start)
    val out = new StringBuilder
    val changes = mutable.ListBuffer[Change]()
    var pos = 0
    var line = 1
    for (c <- used) {
      val before = a.text.substring(pos, c.hit.start)
      line += before.count(_ == '\n')
      val after = c.rule.replacement(c.hit, c.rule.defn.id, book)
      out.append(before).append(after)
      changes += Change(line, c.rule.defn.id, c.hit.text, after)
      line += c.hit.text.count(_ == '\n')
      pos = c.hit.end
    }
    out.append(a.text.substring(pos))
    (out.toString, changes.toList)
  }

  def lineOf(text: String, pos: Int): Int = text.substring(0, pos).count(_ == '\n') + 1
}

// ===== Поиск файлов =====

// path — полный путь, relative — путь внутри выбранного каталога (для структуры результата)
case class SourceFile(path: Path, relative: Path)

// skipped — пары (имя, причина)
case class Scan(files: List[SourceFile], skipped: List[(String, String)])

object Scanner {
  def scan(paths: List[Path], recursive: Boolean, cfg: AppConfig): Scan = {
    val outDir = Paths.get(cfg.outputDirectory).toAbsolutePath.normalize
    val repDir = Paths.get(cfg.reportDirectory).toAbsolutePath.normalize
    val files = mutable.ListBuffer[SourceFile]()
    val skipped = mutable.ListBuffer[(String, String)]()

    def consider(file: Path, relative: Path): Unit = {
      val abs = file.toAbsolutePath.normalize
      val name = file.getFileName.toString.toLowerCase
      if (abs.startsWith(outDir) || abs.startsWith(repDir)) {
        () // свои результаты не обрабатываем
      } else if (!cfg.supportedExtensions.exists(e => name.endsWith(e.toLowerCase))) {
        skipped += ((relative.toString, "неподдерживаемое расширение"))
      } else {
        files += SourceFile(abs, relative)
      }
    }

    for (p <- paths) {
      val abs = p.toAbsolutePath.normalize
      val folder = if (Files.isDirectory(p)) abs else abs.getParent
      if (folder == outDir) throw new ConfigException("Выходной каталог совпадает с исходным")
      if (!Files.exists(p)) {
        skipped += ((p.toString, "файл или каталог не найден"))
      } else if (Files.isDirectory(p)) {
        val stream = if (recursive) Files.walk(p) else Files.list(p)
        try {
          stream.iterator.asScala.filter(f => Files.isRegularFile(f)).foreach(f => consider(f, p.relativize(f)))
        } finally {
          stream.close()
        }
      } else {
        consider(p, p.getFileName)
      }
    }

    Scan(files.toList.distinctBy(_.path).sortBy(_.path.toString), skipped.toList)
  }
}

// ===== Чтение, анализ и запись файлов =====

// Результат анализа одного файла. stamp — (время изменения, размер) для контроля изменений.
case class FileOutcome(
    source: SourceFile,
    analysis: Option[TextAnalysis],
    error: Option[String],
    stamp: (Long, Long))

case class Report(
    policy: String,
    included: List[String],
    overrides: List[String],
    started: LocalDateTime,
    finished: LocalDateTime,
    processed: Int,
    skipped: Int,
    replacements: List[(String, Int)], // (id правила, количество замен)
    conflicts: Int,
    errors: List[(String, String)],    // (файл, причина)
    outputDir: String) {

  def complete: Boolean = errors.isEmpty

  def toText: String = {
    val fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val lines = mutable.ListBuffer[String]()
    lines += (if (complete) "Обработка завершена." else "Обработка завершена, РЕЗУЛЬТАТ НЕПОЛНЫЙ.")
    lines += s"Политика: $policy"
    lines += s"Включённые политики: ${included.mkString(", ")}"
    overrides.foreach(o => lines += s"Переопределение: $o")
    lines += s"Начало: ${started.format(fmt)}"
    lines += s"Завершение: ${finished.format(fmt)}"
    lines += s"Обработано файлов: $processed"
    lines += s"Пропущено файлов: $skipped"
    lines += s"Создано замен: ${replacements.map(_._2).sum}"
    replacements.foreach { case (id, n) => lines += s"  правило $id: $n" }
    lines += s"Конфликтов: $conflicts"
    lines += s"Ошибок: ${errors.size}"
    errors.foreach { case (file, why) => lines += s"  $file: $why" }
    lines += "Результат сохранён:"
    lines += outputDir
    lines.mkString("\n")
  }
}

object Processor {

  private def stampOf(p: Path): (Long, Long) = (Files.getLastModifiedTime(p).toMillis, Files.size(p))

  // Понятная причина ошибки без технических деталей и без содержимого файлов
  def describe(e: Throwable): String = e match {
    case _: CharacterCodingException => "файл содержит данные в неподдерживаемой кодировке"
    case c: ConfigException => c.getMessage
    case _: java.io.IOException => "ошибка чтения или записи файла"
    case _ => "внутренняя ошибка"
  }

  // Строгое чтение UTF-8: неверные байты вызывают ошибку, а не молча портятся
  private def readUtf8(path: Path): String = {
    val decoder = StandardCharsets.UTF_8.newDecoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)
    decoder.decode(ByteBuffer.wrap(Files.readAllBytes(path))).toString
  }

  // Анализ всех файлов. Ошибка одного файла не останавливает остальные.
  def analyzeAll(scan: Scan, rules: List[Rule]): List[FileOutcome] =
    scan.files.map { f =>
      val result = Try {
        val stamp = stampOf(f.path)
        (stamp, Engine.analyze(readUtf8(f.path), rules))
      }
      result match {
        case Success((stamp, analysis)) => FileOutcome(f, Some(analysis), None, stamp)
        case Failure(e) =>
          Log.error(s"Ошибка чтения файла '${f.relative}'")
          FileOutcome(f, None, Some(describe(e)), (0L, 0L))
      }
    }

  // Создаёт анонимизированные копии. Исходные файлы не изменяются.
  def writeCopies(
      policy: String,
      resolved: ResolvedPolicy,
      scan: Scan,
      outcomes: List[FileOutcome],
      cfg: AppConfig,
      withDisputed: Boolean): Report = {

    val started = LocalDateTime.now()
    val outDir = Paths.get(cfg.outputDirectory).toAbsolutePath.normalize
    val book = new PseudonymBook // один набор псевдонимов на всю операцию
    val counts = mutable.LinkedHashMap[String, Int]()
    val errors = mutable.ListBuffer[(String, String)]()
    var processed = 0
    var conflicts = 0

    for (o <- outcomes) {
      val name = o.source.relative.toString
      o.analysis match {
        case None =>
          errors += ((name, o.error.getOrElse("ошибка чтения")))
        case Some(a) =>
          conflicts += a.disputed.size
          val written = Try {
            val (text, changes) = Engine.render(a, withDisputed, book)
            val rel = if (cfg.preserveDirectoryStructure) o.source.relative else o.source.relative.getFileName
            val target = outDir.resolve(rel)
            if (Files.exists(target) && !cfg.overwriteExistingFiles)
              throw new ConfigException("выходной файл уже существует")
            if (stampOf(o.source.path) != o.stamp)
              throw new ConfigException("исходный файл изменился во время обработки")
            Files.createDirectories(target.getParent)
            Files.write(target, text.getBytes(StandardCharsets.UTF_8))
            changes
          }
          written match {
            case Success(changes) =>
              processed += 1
              changes.foreach(c => counts(c.ruleId) = counts.getOrElse(c.ruleId, 0) + 1)
            case Failure(e) =>
              Log.error(s"Ошибка обработки файла '$name'")
              errors += ((name, describe(e)))
          }
      }
    }

    Report(policy, resolved.policies, resolved.overrides, started, LocalDateTime.now(),
      processed, scan.skipped.size, counts.toList, conflicts, errors.toList, outDir.toString)
  }
}
