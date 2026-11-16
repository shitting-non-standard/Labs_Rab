// ===== Журнал (Singleton), рабочая папка и хранение конфигурации =====

import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.file.{Files, InvalidPathException, Path, Paths}
import java.time.LocalDateTime
import scala.collection.immutable.ListMap
import scala.jdk.CollectionConverters.*
import scala.util.Try
import tools.jackson.databind.json.JsonMapper

// Singleton: базовая папка. Относительные пути из конфигурации (словари, журнал, отчёты, выходная папка)
object Workspace {
  var baseDir: Path = Paths.get("").toAbsolutePath
  def resolve(path: String): Path = baseDir.resolve(path).normalize
}

// Singleton: в приложении один журнал, доступный отовсюду.
// ВАЖНО: в журнал пишем только служебные сообщения: id правил, количество, относительные пути.
// Найденные значения, фрагменты документов и ввод пользователя в журнал не попадают.
object Log {
  private var file: Option[Path] = None // None — журнал не работает

  // Запускает журнал. Если файл недоступен, возвращает ошибку, а программа работает без журнала.
  def init(path: String): Either[AppError, Unit] = {
    val started = Safe {
      val p = Workspace.resolve(path)
      Option(p.getParent).foreach(d => Files.createDirectories(d))
      appendText(p, "") // создаём файл, если его нет
      p
    } {
      case _: Exception => LogUnavailable
    }
    file = started.toOption
    started.map(_ => ())
  }

  def info(message: String): Unit = write("INFO", message)
  def warn(message: String): Unit = write("WARN", message)
  def error(message: String): Unit = write("ERROR", message)

  // Дописывает текст в конец файла (UTF-8)
  private def appendText(p: Path, text: String): Unit = {
    val out = new OutputStreamWriter(new FileOutputStream(p.toFile, true), "UTF-8")
    try out.write(text)
    finally out.close()
  }

  // Сбой записи в журнал не должен останавливать программу, поэтому результат намеренно не используется
  private def write(level: String, message: String): Unit =
    file.foreach { p =>
      Try(appendText(p, s"${LocalDateTime.now().withNano(0)} [$level] $message\n"))
    }
}

// Чтение и запись конфигурационного файла.
// JSON читается в обычные Java-коллекции.
// LinkedHashMap сохраняет порядок полей, как в файле.
object ConfigStore {

  private val mapper = JsonMapper.builder().build()

  // Читает конфигурацию и проверяет её целиком
  def loadValidated(path: Path): Either[AppError, AppConfig] =
    for {
      cfg <- load(path)
      _ <- Validator.ensureConfigValid(cfg)
    } yield cfg

  def load(path: Path): Either[AppError, AppConfig] =
    for {
      text <- Safe(Files.readString(path)) { case _: Exception => ConfigUnreadable(path.toString) }
      root <- Safe(mapper.readValue(text, classOf[Object])) { case _: Exception => ConfigMalformed }
      cfg <- parse(root)
    } yield cfg

  def save(path: Path, cfg: AppConfig): Either[AppError, Unit] =
    Safe {
      // сначала целиком готовим текст: ошибка сериализации не испортит старый файл
      val text = mapper.writerWithDefaultPrettyPrinter().writeValueAsString(toJson(cfg))
      Files.writeString(path, text)
      ()
    } {
      case _: Exception => ConfigSaveFailed
    }

  // ---------- Чтение значений из JSON с проверкой типа ----------

  private type Obj = ListMap[String, Any]

  private def asObj(owner: String, field: String, value: Any): Either[AppError, Obj] = value match {
    case m: java.util.Map[?, ?] =>
      Right(ListMap.from(m.asInstanceOf[java.util.Map[String, Any]].asScala))
    case _ => Left(ConfigFieldWrongType(owner, field))
  }

  // Поля одного JSON-объекта. owner нужен только для сообщения об ошибке.
  private class Fields(owner: String, obj: Obj) {
    private def wrong(key: String): AppError = ConfigFieldWrongType(owner, key)
    private def missing(key: String): AppError = ConfigFieldMissing(owner, key)

    private def toStr(key: String)(v: Any): Either[AppError, String] = v match {
      case s: String => Right(s)
      case _ => Left(wrong(key))
    }
    private def toBool(key: String)(v: Any): Either[AppError, Boolean] = v match {
      case b: Boolean => Right(b)
      case _ => Left(wrong(key))
    }
    private def toInt(key: String)(v: Any): Either[AppError, Int] = v match {
      case n: Number => Right(n.intValue)
      case _ => Left(wrong(key))
    }
    private def toList(key: String)(v: Any): Either[AppError, List[Any]] = v match {
      case l: java.util.List[?] => Right(l.asInstanceOf[java.util.List[Any]].asScala.toList)
      case _ => Left(wrong(key))
    }

    def optString(key: String): Either[AppError, Option[String]] =
      Eithers.sequenceOption(obj.get(key).map(v => toStr(key)(v)))

    def string(key: String, default: String): Either[AppError, String] =
      optString(key).map(_.getOrElse(default))

    def requiredString(key: String): Either[AppError, String] =
      optString(key).flatMap(_.toRight(missing(key)))

    def bool(key: String, default: Boolean): Either[AppError, Boolean] =
      Eithers.sequenceOption(obj.get(key).map(v => toBool(key)(v))).map(_.getOrElse(default))

    def int(key: String, default: Int): Either[AppError, Int] =
      Eithers.sequenceOption(obj.get(key).map(v => toInt(key)(v))).map(_.getOrElse(default))

    // Список строк; если поля нет — значение по умолчанию
    def strings(key: String, default: List[String]): Either[AppError, List[String]] =
      Eithers.sequenceOption(
        obj.get(key).map(v => toList(key)(v).flatMap(items => Eithers.traverse(items)(item => toStr(key)(item))))
      ).map(_.getOrElse(default))

    // Список JSON-объектов; если поля нет — пустой список
    def objects(key: String): Either[AppError, List[Obj]] =
      Eithers.sequenceOption(
        obj.get(key).map(v => toList(key)(v).flatMap(items => Eithers.traverse(items)(item => asObj(owner, key, item))))
      ).map(_.getOrElse(Nil))

    def requiredObject(key: String): Either[AppError, Obj] =
      obj.get(key).toRight(missing(key)).flatMap(v => asObj(owner, key, v))

    // Вложенный объект, значения которого тоже объекты (имя -> объект)
    def members(key: String): Either[AppError, List[(String, Obj)]] =
      Eithers.sequenceOption(
        obj.get(key).map(v => asObj(owner, key, v).flatMap(inner =>
          Eithers.traverse(inner.toList) { case (name, body) => asObj(key, name, body).map(o => name -> o) }))
      ).map(_.getOrElse(Nil))
  }

  // ---------- JSON -> объекты ----------

  private def parse(root: Any): Either[AppError, AppConfig] =
    for {
      o <- asObj("конфигурация", "корень", root)
      f = new Fields("Конфигурация", o)
      logFile <- f.string("logFile", "anonymizer.log")
      reportDirectory <- f.string("reportDirectory", "reports")
      outputDirectory <- f.string("outputDirectory", "anonymized")
      _ <- Eithers.traverse(List("logFile" -> logFile, "reportDirectory" -> reportDirectory, "outputDirectory" -> outputDirectory)) {
        case (field, value) => checkPath(field, value)
      }
      defaultPolicy <- f.requiredString("defaultPolicy")
      preserve <- f.bool("preserveDirectoryStructure", true)
      overwrite <- f.bool("overwriteExistingFiles", false)
      extensions <- f.strings("supportedExtensions", List(".txt", ".md"))
      entries <- f.members("policies")
      policies <- Eithers.traverse(entries) { case (name, body) => parsePolicy(name, body).map(p => name -> p) }
    } yield AppConfig(logFile, reportDirectory, outputDirectory, defaultPolicy, preserve, overwrite, extensions,
      ListMap.from(policies))
  
  private def checkPath(field: String, value: String): Either[AppError, Unit] =
    Safe {
      Workspace.resolve(value)
      ()
    } {
      case _: InvalidPathException => ConfigFieldWrongType("Конфигурация", field)
    }

  private def parsePolicy(name: String, o: Obj): Either[AppError, Policy] = {
    val f = new Fields(s"Политика '$name'", o)
    for {
      description <- f.optString("description")
      includes <- f.strings("includes", Nil)
      ruleObjects <- f.objects("rules")
      rules <- Eithers.traverse(ruleObjects)(r => parseRule(name, r))
    } yield Policy(description, includes, rules)
  }

  private def parseRule(policy: String, o: Obj): Either[AppError, RuleDef] =
    for {
      id <- new Fields(s"Политика '$policy' (правило)", o).requiredString("id")
      f = new Fields(s"Правило '$id'", o)
      description <- f.optString("description")
      matcherObj <- f.requiredObject("matcher")
      replacementObj <- f.requiredObject("replacement")
      // приоритет должен быть числом, иначе это отдельная ошибка конфигурации
      priority <- o.get("priority").collect { case n: Number => n.intValue }.toRight(PriorityNotANumber(id))
      enabled <- f.bool("enabled", true)
      mf = new Fields(s"Правило '$id', поиск", matcherObj)
      matcherKind <- mf.requiredString("type")
      matcherValue <- mf.requiredString("value")
      matcher <- MatcherKind.parse(matcherKind)
        .flatMap(kind => MatcherSpec.parse(kind, matcherValue))
        .left.map(e => InvalidRule(id, e))
      rf = new Fields(s"Правило '$id', замена", replacementObj)
      replacementKind <- rf.requiredString("type")
      value <- rf.optString("value")
      template <- rf.optString("template")
      values <- rf.strings("values", Nil)
      keepStart <- rf.int("keepStart", 0)
      keepEnd <- rf.int("keepEnd", 0)
      replacement <- ReplacementKind.parse(replacementKind)
        .flatMap(kind => ReplacementSpec.parse(kind, value, template, values, keepStart, keepEnd))
        .left.map(e => InvalidRule(id, e))
    } yield RuleDef(id, description, matcher, replacement, priority, enabled)

  // ---------- объекты -> JSON (обычные Java-коллекции) ----------

  private def jmap(fields: Seq[(String, Any)]): java.util.Map[String, Any] = {
    val m = new java.util.LinkedHashMap[String, Any]()
    fields.foreach { case (k, v) => m.put(k, v) }
    m
  }

  private def jlist(items: Seq[Any]): java.util.List[Any] = {
    val l = new java.util.ArrayList[Any]()
    items.foreach(i => l.add(i))
    l
  }

  private def toJson(c: AppConfig): java.util.Map[String, Any] =
    jmap(List[(String, Any)](
      "logFile" -> c.logFile,
      "reportDirectory" -> c.reportDirectory,
      "outputDirectory" -> c.outputDirectory,
      "defaultPolicy" -> c.defaultPolicy,
      "preserveDirectoryStructure" -> c.preserveDirectoryStructure,
      "overwriteExistingFiles" -> c.overwriteExistingFiles,
      "supportedExtensions" -> jlist(c.supportedExtensions),
      "policies" -> jmap(c.policies.toList.map { case (name, p) => name -> policyToJson(p) })))

  private def policyToJson(p: Policy): java.util.Map[String, Any] = {
    val description: List[(String, Any)] = p.description.map(d => "description" -> d).toList
    val includes: List[(String, Any)] = if (p.includes.isEmpty) Nil else List("includes" -> jlist(p.includes))
    jmap(description ++ includes ++ List[(String, Any)]("rules" -> jlist(p.rules.map(r => ruleToJson(r)))))
  }

  private def ruleToJson(r: RuleDef): java.util.Map[String, Any] =
    val description: List[(String, Any)] = r.description.map(d => "description" -> d).toList
    jmap(
      List[(String, Any)]("id" -> r.id) ++
        description ++
        List[(String, Any)](
          "matcher" -> matcherJson(r.matcher),
          "replacement" -> replacementJson(r.replacement),
          "priority" -> r.priority,
          "enabled" -> r.enabled))

  private def matcherJson(spec: MatcherSpec): java.util.Map[String, Any] = spec match {
    case ExactSpec(value) => jmap(List("type" -> "exact", "value" -> value))
    case RegexSpec(pattern) => jmap(List("type" -> "regex", "value" -> pattern))
    case DictionarySpec(path) => jmap(List("type" -> "dictionary", "value" -> path))
    case BuiltinSpec(builtin) => jmap(List("type" -> "builtin", "value" -> builtin.key))
  }

  private def replacementJson(spec: ReplacementSpec): java.util.Map[String, Any] = spec match {
    case ConstantSpec(value) => jmap(List("type" -> "constant", "value" -> value))
    case RemoveSpec => jmap(List("type" -> "remove"))
    case MaskSpec(start, end) => jmap(List[(String, Any)]("type" -> "mask", "keepStart" -> start, "keepEnd" -> end))
    case PseudonymSpec(template) => jmap(List("type" -> "pseudonym", "template" -> template))
    case ListSpec(values) => jmap(List[(String, Any)]("type" -> "list", "values" -> jlist(values)))
  }
}
