// ===== Журнал (Singleton) и хранение конфигурации =====

import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*
import tools.jackson.databind.json.JsonMapper

// Singleton: базовая папка. Относительные пути из конфигурации (словари, журнал, отчёты,
// выходная папка) считаются от папки, где лежит файл конфигурации, а не от папки запуска.
object Workspace {
  var baseDir: Path = Paths.get("").toAbsolutePath
  def resolve(path: String): Path = baseDir.resolve(path).normalize
}

// Singleton: в приложении один журнал, доступный отовсюду.
// ВАЖНО: в журнал пишем только служебные сообщения: id правил, количество, относительные пути.
// Найденные значения, фрагменты документов и ввод пользователя в журнал не попадают.
object Log {
  private var file: Option[Path] = None

  def init(path: String): Unit = {
    try {
      val p = Workspace.resolve(path)
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

// Чтение и запись конфигурационного файла (JSON, библиотека Jackson 3).
// JSON читается в обычные Java-коллекции (Map, List, String, Number, Boolean):
// LinkedHashMap сохраняет порядок полей, как в файле.
object ConfigStore {

  private val mapper = JsonMapper.builder().build()

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
      parse(mapper.readValue(text, classOf[Object]))
    } catch {
      case e: ConfigException => throw e
      case _: Exception => throw new ConfigException("Конфигурационный файл имеет неправильный формат")
    }
  }

  def save(path: Path, cfg: AppConfig): Unit = {
    try {
      // сначала целиком готовим текст: ошибка сериализации не испортит старый файл
      val text = mapper.writerWithDefaultPrettyPrinter().writeValueAsString(toJson(cfg))
      Files.writeString(path, text)
    } catch {
      case _: java.io.IOException => throw new ConfigException("Не удалось сохранить конфигурацию")
    }
  }

  // ---------- Доступ к данным с проверкой типа ----------

  private type Obj = ListMap[String, Any]

  private def wrongStructure: Nothing =
    throw new ConfigException("Конфигурационный файл имеет неправильную структуру")

  private def asObj(v: Any): Obj = v match {
    case m: java.util.Map[?, ?] => ListMap.from(m.asInstanceOf[java.util.Map[String, Any]].asScala)
    case _ => wrongStructure
  }
  private def asList(v: Any): List[Any] = v match {
    case l: java.util.List[?] => l.asInstanceOf[java.util.List[Any]].asScala.toList
    case _ => wrongStructure
  }
  private def asStr(v: Any): String = v match { case s: String => s; case _ => wrongStructure }
  private def asBool(v: Any): Boolean = v match { case b: Boolean => b; case _ => wrongStructure }
  private def asInt(v: Any): Int = v match { case n: Number => n.intValue; case _ => wrongStructure }
  private def asStrList(v: Any): List[String] = asList(v).map(x => asStr(x))

  // ---------- JSON -> объекты ----------

  private def parse(root: Any): AppConfig = {
    val o = asObj(root)
    def str(key: String, default: String): String = o.get(key).map(v => asStr(v)).getOrElse(default)
    def bool(key: String, default: Boolean): Boolean = o.get(key).map(v => asBool(v)).getOrElse(default)

    val policies = ListMap.from(
      o.get("policies").map(p => asObj(p).toList).getOrElse(Nil).map { case (name, v) =>
        name -> parsePolicy(name, asObj(v))
      })

    AppConfig(
      logFile = str("logFile", "anonymizer.log"),
      reportDirectory = str("reportDirectory", "reports"),
      outputDirectory = str("outputDirectory", "anonymized"),
      defaultPolicy = str("defaultPolicy", ""),
      preserveDirectoryStructure = bool("preserveDirectoryStructure", true),
      overwriteExistingFiles = bool("overwriteExistingFiles", false),
      supportedExtensions = o.get("supportedExtensions").map(e => asStrList(e)).getOrElse(List(".txt", ".md")),
      policies = policies)
  }

  private def parsePolicy(name: String, o: Obj): Policy =
    Policy(
      description = o.get("description").map(v => asStr(v)).getOrElse(""),
      includes = o.get("includes").map(v => asStrList(v)).getOrElse(Nil),
      rules = o.get("rules").map(a => asList(a).map(r => parseRule(name, asObj(r)))).getOrElse(Nil))

  private def parseRule(policy: String, o: Obj): RuleDef = {
    def field(m: Obj, key: String, what: String): Any =
      m.getOrElse(key, throw new ConfigException(s"Политика '$policy': у $what нет поля '$key'"))

    val id = asStr(field(o, "id", "правила"))
    val where = s"правила '$id'"
    val m = asObj(field(o, "matcher", where))
    val r = asObj(field(o, "replacement", where))

    // приоритет должен быть числом, иначе это ошибка конфигурации
    val priority = o.get("priority") match {
      case Some(n: Number) => n.intValue
      case _ => throw new ConfigException(s"Правило '$id': приоритет не является числом")
    }

    RuleDef(
      id = id,
      description = o.get("description").map(v => asStr(v)).getOrElse(""),
      matcher = MatcherSpec(asStr(field(m, "type", where)), asStr(field(m, "value", where))),
      replacement = ReplacementSpec(
        kind = asStr(field(r, "type", where)),
        value = r.get("value").map(v => asStr(v)),
        template = r.get("template").map(v => asStr(v)),
        values = r.get("values").map(v => asStrList(v)).getOrElse(Nil),
        keepStart = r.get("keepStart").map(v => asInt(v)).getOrElse(0),
        keepEnd = r.get("keepEnd").map(v => asInt(v)).getOrElse(0)),
      priority = priority,
      enabled = o.get("enabled").map(v => asBool(v)).getOrElse(true))
  }

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
    val fields = ListBuffer[(String, Any)]()
    fields += (("description", p.description))
    if (p.includes.nonEmpty) fields += (("includes", jlist(p.includes)))
    fields += (("rules", jlist(p.rules.map(r => ruleToJson(r)))))
    jmap(fields.toList)
  }

  private def ruleToJson(r: RuleDef): java.util.Map[String, Any] = {
    val repl = ListBuffer[(String, Any)]()
    repl += (("type", r.replacement.kind))
    r.replacement.value.foreach(v => repl += (("value", v)))
    r.replacement.template.foreach(t => repl += (("template", t)))
    if (r.replacement.values.nonEmpty) repl += (("values", jlist(r.replacement.values)))
    if (r.replacement.kind == "mask") {
      repl += (("keepStart", r.replacement.keepStart))
      repl += (("keepEnd", r.replacement.keepEnd))
    }
    jmap(List[(String, Any)](
      "id" -> r.id,
      "description" -> r.description,
      "matcher" -> jmap(List[(String, Any)]("type" -> r.matcher.kind, "value" -> r.matcher.value)),
      "replacement" -> jmap(repl.toList),
      "priority" -> r.priority,
      "enabled" -> r.enabled))
  }
}
