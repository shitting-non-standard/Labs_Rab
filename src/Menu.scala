// ===== Меню и регистр команд =====
// Пункт меню (MenuItem) хранит имя, подсказку, описание и функцию РАЗБОРА аргументов,
// которая возвращает типизированное значение A (команду) или ошибку.
// Меню неизменяемо: add возвращает новое меню. Справка строится из регистра автоматически.

import scala.collection.immutable.ListMap

case class MenuItem[A](
    name: String,
    usage: String,
    description: String,
    parse: List[String] => Either[AppError, A])

case class Menu[A](title: String, items: ListMap[String, MenuItem[A]] = ListMap.empty[String, MenuItem[A]]) {

  // Новое меню с добавленным пунктом (пункт с тем же именем заменяется)
  def add(item: MenuItem[A]): Menu[A] = copy(items = items.updated(item.name, item))

  def find(name: String): Option[MenuItem[A]] = items.get(name)

  // Разбирает уже разделённую на слова строку: первое слово — имя пункта, остальное — аргументы
  def parse(tokens: List[String]): Either[AppError, A] = tokens match {
    case name :: args => find(name).toRight(UnknownCommand).flatMap(item => item.parse(args))
    case Nil => Left(UnknownCommand)
  }

  // Строки справки: подсказка и описание выровнены в две колонки
  def lines: List[String] = {
    val width = items.values.map(_.usage.length).maxOption.getOrElse(0)
    (title + ":") :: items.values.toList.map(i => "  " + i.usage.padTo(width, ' ') + "  " + i.description)
  }

  def print(): Unit = lines.foreach(l => println(l))
}
