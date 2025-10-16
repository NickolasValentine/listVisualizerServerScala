package data

// Функциональные интерфейсы для callback-операций
trait TestIt {
  def testIt(obj: AnyRef): Boolean
}