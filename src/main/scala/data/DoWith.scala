package data

// Функциональные интерфейсы для callback-операций
trait DoWith {
  def doWith(obj: AnyRef): Unit
}