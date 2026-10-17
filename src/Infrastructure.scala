// ===== Журнал (Singleton) и хранение конфигурации =====

import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer

// Singleton: в приложении один журнал, доступный отовсюду.

object Log {
  private var file: Option[Path] = None

  def init(path: String): Unit = {
    try {
      val p = Paths.get(path).toAbsolutePath
      Option(p.getParent).foreach(d => Files.createDirectories(d))
      appendText(p, "") // создаём файл, если его нет
      file = Some(p)
    } catch {
      case _: Exception =>
        file = None
        println("Предупреждение: не удалось создать журнал, работа продолжится без него.")
    }
  }

  // Дописывает текст в конец файла (UTF-8)
  private def appendText(p: Path, text: String): Unit = {
    val out = new OutputStreamWriter(new FileOutputStream(p.toFile, true), "UTF-8")
    try out.write(text)
    finally out.close()
  }

  def info(message: String): Unit = write("INFO", message)
  def warn(message: String): Unit = write("WARN", message)
  def error(message: String): Unit = write("ERROR", message)

  private def write(level: String, message: String): Unit =
    file.foreach { p =>
      try {
        val line = s"${LocalDateTime.now().withNano(0)} [$level] $message\n"
        appendText(p, line)
      } catch {
        case _: java.io.IOException => () // журнал недоступен — работу не прерываем
      }
    }
}

// Чтение и запись конфигурационного файла
object ConfigStore {

  // Читает и проверяет конфигурацию. При любой проблеме бросает ConfigException.
  def loadValidated(path: Path): AppConfig = {
    val cfg = load(path)
    val errors = Validator.validateConfig(cfg)
    if (errors.nonEmpty) throw new ConfigException(errors.map(e => "  - " + e).mkString("\n"))
    cfg
  }

  def load(path: Path): AppConfig = {
    val text =
      try {
        Files.readString(path)
      } catch {
        case _: Exception => throw new ConfigException(s"Не удалось прочитать файл конфигурации '$path'")
      }
    try {
      parse(Json.parse(text))
    } catch {
      case e: ConfigException => throw e
      case _: Exception => throw new ConfigException("Конфигурационный файл имеет неправильный формат")
    }
  }

  def save(path: Path, cfg: AppConfig): Unit = {
    try {
      // сначала целиком готовим текст: ошибка сериализации не испортит старый файл
      val text = Json.write(toJson(cfg))
      Files.writeString(path, text)
    } catch {
      case _: java.io.IOException => throw new ConfigException("Не удалось сохранить конфигурацию")
    }
  }

  // ---------- JSON -> объекты ----------

  private def parse(json: Json): AppConfig = {
    val o = Json.obj(json)
    def str(key: String, default: String): String = o.get(key).map(v => Json.str(v)).getOrElse(default)
    def bool(key: String, default: Boolean): Boolean = o.get(key).map(v => Json.bool(v)).getOrElse(default)

    val policies = ListMap.from(
      o.get("policies").map(p => Json.obj(p).fields).getOrElse(Nil).map { case (name, v) =>
        name -> parsePolicy(name, Json.obj(v))
      })

    AppConfig(
      logFile = str("logFile", "anonymizer.log"),
      reportDirectory = str("reportDirectory", "reports"),
      outputDirectory = str("outputDirectory", "anonymized"),
      defaultPolicy = str("defaultPolicy", ""),
      preserveDirectoryStructure = bool("preserveDirectoryStructure", true),
      overwriteExistingFiles = bool("overwriteExistingFiles", false),
      supportedExtensions = o.get("supportedExtensions")
        .map(e => Json.arr(e).map(x => Json.str(x))).getOrElse(List(".txt", ".md")),
      policies = policies)
  }

  private def parsePolicy(name: String, o: JObj): Policy =
    Policy(
      description = o.get("description").map(v => Json.str(v)).getOrElse(""),
      includes = o.get("includes").map(a => Json.arr(a).map(x => Json.str(x))).getOrElse(Nil),
      rules = o.get("rules").map(a => Json.arr(a).map(r => parseRule(name, Json.obj(r)))).getOrElse(Nil))

  private def parseRule(policy: String, o: JObj): RuleDef = {
    def field(m: JObj, key: String, what: String): Json =
      m.get(key).getOrElse(throw new ConfigException(s"Политика '$policy': у $what нет поля '$key'"))

    val id = Json.str(field(o, "id", "правила"))
    val where = s"правила '$id'"
    val m = Json.obj(field(o, "matcher", where))
    val r = Json.obj(field(o, "replacement", where))

    // приоритет должен быть числом, иначе это ошибка конфигурации
    val priority = o.get("priority") match {
      case Some(JNum(d)) => d.toInt
      case _ => throw new ConfigException(s"Правило '$id': приоритет не является числом")
    }

    RuleDef(
      id = id,
      description = o.get("description").map(v => Json.str(v)).getOrElse(""),
      matcher = MatcherSpec(Json.str(field(m, "type", where)), Json.str(field(m, "value", where))),
      replacement = ReplacementSpec(
        kind = Json.str(field(r, "type", where)),
        value = r.get("value").map(v => Json.str(v)),
        template = r.get("template").map(v => Json.str(v)),
        values = r.get("values").map(a => Json.arr(a).map(x => Json.str(x))).getOrElse(Nil),
        keepStart = r.get("keepStart").map(n => Json.num(n).toInt).getOrElse(0),
        keepEnd = r.get("keepEnd").map(n => Json.num(n).toInt).getOrElse(0)),
      priority = priority,
      enabled = o.get("enabled").map(v => Json.bool(v)).getOrElse(true))
  }

  // ---------- объекты -> JSON ----------

  private def strArray(xs: List[String]): Json = JArr(xs.map(x => JStr(x)))

  private def toJson(c: AppConfig): Json =
    JObj(List[(String, Json)](
      "logFile" -> JStr(c.logFile),
      "reportDirectory" -> JStr(c.reportDirectory),
      "outputDirectory" -> JStr(c.outputDirectory),
      "defaultPolicy" -> JStr(c.defaultPolicy),
      "preserveDirectoryStructure" -> JBool(c.preserveDirectoryStructure),
      "overwriteExistingFiles" -> JBool(c.overwriteExistingFiles),
      "supportedExtensions" -> strArray(c.supportedExtensions),
      "policies" -> JObj(c.policies.toList.map { case (name, p) => name -> policyToJson(p) })))

  private def policyToJson(p: Policy): Json = {
    val fields = ListBuffer[(String, Json)]()
    fields += (("description", JStr(p.description)))
    if (p.includes.nonEmpty) fields += (("includes", strArray(p.includes)))
    fields += (("rules", JArr(p.rules.map(r => ruleToJson(r)))))
    JObj(fields.toList)
  }

  private def ruleToJson(r: RuleDef): Json = {
    val repl = ListBuffer[(String, Json)]()
    repl += (("type", JStr(r.replacement.kind)))
    r.replacement.value.foreach(v => repl += (("value", JStr(v))))
    r.replacement.template.foreach(t => repl += (("template", JStr(t))))
    if (r.replacement.values.nonEmpty) repl += (("values", strArray(r.replacement.values)))
    if (r.replacement.kind == "mask") {
      repl += (("keepStart", JNum(r.replacement.keepStart.toDouble)))
      repl += (("keepEnd", JNum(r.replacement.keepEnd.toDouble)))
    }
    JObj(List[(String, Json)](
      "id" -> JStr(r.id),
      "description" -> JStr(r.description),
      "matcher" -> JObj(List[(String, Json)](
        "type" -> JStr(r.matcher.kind),
        "value" -> JStr(r.matcher.value))),
      "replacement" -> JObj(repl.toList),
      "priority" -> JNum(r.priority.toDouble),
      "enabled" -> JBool(r.enabled)))
  }
}
