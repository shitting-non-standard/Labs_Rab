// ===== ADT команд и регистр команд =====
// Строка ввода разбирается ОДИН раз в значение типа Command. Дальше программа работает только с ним.

import java.nio.file.{InvalidPathException, Path, Paths}

// Что нужно найти: политика + пути (пустой список — взять выбранные командой select) + режим -r
case class ScanRequest(policy: String, paths: List[Path], recursive: Boolean)

// Файлы и каталоги, выбранные командой select
case class Selection(paths: List[Path], recursive: Boolean)

// ---------- Команды ----------
sealed trait Command
case object ShowHelp extends Command
case object ListPolicies extends Command
case class ShowPolicy(name: String) extends Command
case object CreatePolicy extends Command
case class EditPolicy(name: String) extends Command
case class DeletePolicy(name: String) extends Command
// includes = None — пользователь не указал политики в команде, их нужно спросить
case class ComposePolicy(name: String, includes: Option[List[String]]) extends Command
case class ValidatePolicy(name: String) extends Command
case class SelectPaths(paths: List[Path], recursive: Boolean) extends Command
case class Preview(request: ScanRequest) extends Command
case class Anonymize(request: ScanRequest) extends Command
case object ShowReport extends Command
case object ReloadConfig extends Command
case object Exit extends Command

// ---------- Действия в меню редактирования политики ----------
sealed trait EditAction
case object AddRule extends EditAction
case object RemoveRule extends EditAction
case object ToggleRule extends EditAction
case object ChangeDescription extends EditAction
case object FinishEditing extends EditAction

object Commands {

  // Разбор строки на слова; текст в кавычках считается одним словом (пути с пробелами)
  private val tokenRe = "\"([^\"]*)\"|(\\S+)".r

  def tokenize(line: String): List[String] =
    tokenRe.findAllMatchIn(line).map(m => Option(m.group(1)).getOrElse(m.group(2))).toList

  // Пустая строка — не команда (None). Иначе — команда или ошибка разбора.
  def parseLine(line: String): Option[Either[AppError, Command]] =
    tokenize(line) match {
      case Nil => None
      case tokens => Some(registry.parse(tokens))
    }

  // ---------- Разбор аргументов ----------

  private def name(what: String)(args: List[String]): Either[AppError, String] =
    args.headOption.toRight(MissingArgument(what))

  private def path(text: String): Either[AppError, Path] =
    Safe(Paths.get(text)) { case _: InvalidPathException => InvalidPath }

  private def paths(args: List[String]): Either[AppError, List[Path]] =
    Eithers.traverse(args.filterNot(_ == "-r"))(a => path(a))

  private def scanRequest(args: List[String]): Either[AppError, ScanRequest] =
    for {
      policy <- name("политику (например: preview <политика> [-r] [пути])")(args)
      found <- paths(args.drop(1))
    } yield ScanRequest(policy, found, args.drop(1).contains("-r"))

  // ---------- Регистр команд ----------
  // Чтобы добавить команду: новый вариант в Command + одна строка .add(...) здесь.

  val registry: Menu[Command] = Menu[Command]("Команды")
    .add(MenuItem("help", "help", "список команд", _ => Right(ShowHelp)))
    .add(MenuItem("policies", "policies", "список политик", _ => Right(ListPolicies)))
    .add(MenuItem("show-policy", "show-policy <имя>", "правила политики",
      args => name("имя политики")(args).map(n => ShowPolicy(n))))
    .add(MenuItem("create-policy", "create-policy", "создать политику", _ => Right(CreatePolicy)))
    .add(MenuItem("edit-policy", "edit-policy <имя>", "изменить политику",
      args => name("имя политики")(args).map(n => EditPolicy(n))))
    .add(MenuItem("delete-policy", "delete-policy <имя>", "удалить политику",
      args => name("имя политики")(args).map(n => DeletePolicy(n))))
    .add(MenuItem("compose-policy", "compose-policy <имя> [политики]", "составить политику из существующих",
      args => name("имя политики")(args).map(n => ComposePolicy(n, Some(args.drop(1)).filter(_.nonEmpty)))))
    .add(MenuItem("validate-policy", "validate-policy <имя>", "проверить политику",
      args => name("имя политики")(args).map(n => ValidatePolicy(n))))
    .add(MenuItem("select", "select [-r] <путь>...", "выбрать файлы или каталоги (-r — с вложенными)",
      args => paths(args).map(ps => SelectPaths(ps, args.contains("-r")))))
    .add(MenuItem("preview", "preview <политика> [-r] [путь]...", "предпросмотр найденных данных",
      args => scanRequest(args).map(r => Preview(r))))
    .add(MenuItem("anonymize", "anonymize <политика> [-r] [путь]...", "создать анонимизированные копии",
      args => scanRequest(args).map(r => Anonymize(r))))
    .add(MenuItem("report", "report", "последний отчёт", _ => Right(ShowReport)))
    .add(MenuItem("reload", "reload", "перечитать конфигурацию", _ => Right(ReloadConfig)))
    .add(MenuItem("exit", "exit", "выход", _ => Right(Exit)))

  // Меню редактирования политики: те же средства, но вместо Command выдаёт EditAction
  val editMenu: Menu[EditAction] = Menu[EditAction]("Редактирование политики")
    .add(MenuItem("1", "1", "добавить правило", _ => Right(AddRule)))
    .add(MenuItem("2", "2", "удалить правило", _ => Right(RemoveRule)))
    .add(MenuItem("3", "3", "включить или выключить правило", _ => Right(ToggleRule)))
    .add(MenuItem("4", "4", "изменить описание", _ => Right(ChangeDescription)))
    .add(MenuItem("0", "0", "сохранить и выйти", _ => Right(FinishEditing)))
}
