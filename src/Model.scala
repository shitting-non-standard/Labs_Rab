// ===== Модель данных =====
// Здесь только описание структур. Логики нет и не будет.

import scala.collection.immutable.ListMap

// Ошибка конфигурации или политики. Текст сообщения безопасен для показа и для журнала.
class ConfigException(message: String) extends Exception(message)

// Способ поиска: exact | regex | dictionary | builtin
case class MatcherSpec(kind: String, value: String)

// Способ замены: constant | remove | mask | pseudonym | list
// Option = "значение может быть не указано" (это ошибка, которую найдёт проверка)
case class ReplacementSpec(
    kind: String,
    value: Option[String] = None,    // constant: строка замены (можно использовать $1, $2 ...)
    template: Option[String] = None, // pseudonym: шаблон, например user{number}@example.com
    values: List[String] = Nil,      // list: значения для подстановки
    keepStart: Int = 0,              // mask: сколько символов оставить в начале
    keepEnd: Int = 0                 // mask: сколько символов оставить в конце
)

// Правило анонимизации (как оно записано в конфигурации)
case class RuleDef(
    id: String,
    description: String,
    matcher: MatcherSpec,
    replacement: ReplacementSpec,
    priority: Int,
    enabled: Boolean
)

// Политика: ссылки на другие политики (includes) + собственные правила
case class Policy(description: String, includes: List[String], rules: List[RuleDef])

// Вся конфигурация приложения
case class AppConfig(
    logFile: String,
    reportDirectory: String,
    outputDirectory: String,
    defaultPolicy: String,
    preserveDirectoryStructure: Boolean,
    overwriteExistingFiles: Boolean,
    supportedExtensions: List[String],
    policies: ListMap[String, Policy] // ListMap хранит порядок, как в файле
)
