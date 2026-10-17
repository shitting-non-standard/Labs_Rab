import scala.collection.mutable

// Значения JSON. JObj хранит поля списком, чтобы сохранялся порядок, как в файле.
sealed trait Json
case class JStr(value: String) extends Json
case class JNum(value: Double) extends Json
case class JBool(value: Boolean) extends Json
case object JNull extends Json
case class JArr(items: List[Json]) extends Json
case class JObj(fields: List[(String, Json)]) extends Json {
  def get(key: String): Option[Json] = fields.find(f => f._1 == key).map(f => f._2)
}

object Json {

  // ---------- Доступ к значениям с проверкой типа ----------

  private def wrongStructure: Nothing =
    throw new ConfigException("Конфигурационный файл имеет неправильную структуру")

  def str(j: Json): String = j match { case JStr(s) => s; case _ => wrongStructure }
  def bool(j: Json): Boolean = j match { case JBool(b) => b; case _ => wrongStructure }
  def num(j: Json): Double = j match { case JNum(n) => n; case _ => wrongStructure }
  def arr(j: Json): List[Json] = j match { case JArr(items) => items; case _ => wrongStructure }
  def obj(j: Json): JObj = j match { case o: JObj => o; case _ => wrongStructure }

  // ---------- Разбор текста ----------

  def parse(text: String): Json = new Parser(text).document()

  private class Parser(s: String) {
    private var i = 0

    private def fail(): Nothing =
      throw new ConfigException("Конфигурационный файл имеет неправильный формат JSON")

    private def skip(): Unit =
      while (i < s.length && s.charAt(i).isWhitespace) i += 1

    private def peek: Char = if (i < s.length) s.charAt(i) else fail()

    def document(): Json = {
      val v = value()
      skip()
      if (i != s.length) fail()
      v
    }

    private def value(): Json = {
      skip()
      peek match {
        case '{' => objectValue()
        case '[' => arrayValue()
        case '"' => JStr(string())
        case 't' => literal("true", JBool(true))
        case 'f' => literal("false", JBool(false))
        case 'n' => literal("null", JNull)
        case _ => number()
      }
    }

    private def literal(word: String, result: Json): Json = {
      if (!s.startsWith(word, i)) fail()
      i += word.length
      result
    }

    private def number(): Json = {
      val start = i
      while (i < s.length && "+-.eE0123456789".indexOf(s.charAt(i).toInt) >= 0) i += 1
      if (start == i) fail()
      try {
        JNum(s.substring(start, i).toDouble)
      } catch {
        case _: NumberFormatException => fail()
      }
    }

    private def string(): String = {
      i += 1 // открывающая кавычка
      val sb = new StringBuilder
      var closed = false
      while (!closed) {
        val c = peek
        i += 1
        if (c == '"') {
          closed = true
        } else if (c == '\\') {
          val e = peek
          i += 1
          e match {
            case '"' => sb.append('"')
            case '\\' => sb.append('\\')
            case '/' => sb.append('/')
            case 'b' => sb.append('\b')
            case 'f' => sb.append('\f')
            case 'n' => sb.append('\n')
            case 'r' => sb.append('\r')
            case 't' => sb.append('\t')
            case 'u' =>
              if (i + 4 > s.length) fail()
              val code =
                try {
                  Integer.parseInt(s.substring(i, i + 4), 16)
                } catch {
                  case _: NumberFormatException => fail()
                }
              sb.append(code.toChar)
              i += 4
            case _ => fail()
          }
        } else {
          sb.append(c)
        }
      }
      sb.toString
    }

    private def arrayValue(): Json = {
      i += 1 // [
      val items = mutable.ListBuffer[Json]()
      skip()
      if (peek == ']') {
        i += 1
      } else {
        var more = true
        while (more) {
          items += value()
          skip()
          peek match {
            case ',' => i += 1
            case ']' =>
              i += 1
              more = false
            case _ => fail()
          }
        }
      }
      JArr(items.toList)
    }

    private def objectValue(): Json = {
      i += 1 // {
      val fields = mutable.ListBuffer[(String, Json)]()
      skip()
      if (peek == '}') {
        i += 1
      } else {
        var more = true
        while (more) {
          skip()
          if (peek != '"') fail()
          val key = string()
          skip()
          if (peek != ':') fail()
          i += 1
          fields += ((key, value()))
          skip()
          peek match {
            case ',' => i += 1
            case '}' =>
              i += 1
              more = false
            case _ => fail()
          }
        }
      }
      JObj(fields.toList)
    }
  }

  // ---------- Запись текста (с отступами) ----------

  def write(j: Json): String = {
    val sb = new StringBuilder
    writeValue(j, 0, sb)
    sb.toString
  }

  private def indent(level: Int): String = "  " * level

  private def writeValue(j: Json, level: Int, sb: StringBuilder): Unit = j match {
    case JStr(v) => quote(v, sb)
    case JNum(v) => sb.append(if (v == v.toLong.toDouble) v.toLong.toString else v.toString)
    case JBool(v) => sb.append(v.toString)
    case JNull => sb.append("null")
    case JArr(items) =>
      if (items.isEmpty) {
        sb.append("[]")
      } else {
        sb.append("[\n")
        items.zipWithIndex.foreach { case (item, n) =>
          sb.append(indent(level + 1))
          writeValue(item, level + 1, sb)
          if (n < items.size - 1) sb.append(",")
          sb.append("\n")
        }
        sb.append(indent(level)).append("]")
      }
    case JObj(fields) =>
      if (fields.isEmpty) {
        sb.append("{}")
      } else {
        sb.append("{\n")
        fields.zipWithIndex.foreach { case ((key, item), n) =>
          sb.append(indent(level + 1))
          quote(key, sb)
          sb.append(": ")
          writeValue(item, level + 1, sb)
          if (n < fields.size - 1) sb.append(",")
          sb.append("\n")
        }
        sb.append(indent(level)).append("}")
      }
  }

  // Строка в кавычках; спецсимволы экранируются, кириллица остаётся как есть
  private def quote(text: String, sb: StringBuilder): Unit = {
    sb.append('"')
    text.foreach { c =>
      if (c == '"') sb.append("\\\"")
      else if (c == '\\') sb.append("\\\\")
      else if (c == '\n') sb.append("\\n")
      else if (c == '\r') sb.append("\\r")
      else if (c == '\t') sb.append("\\t")
      else if (c < ' ') sb.append("\\u" + "%04x".format(c.toInt))
      else sb.append(c)
    }
    sb.append('"')
  }
}
