package data

// Компаратор для сравнения двух объектов
trait Comparator {
  def compare(o1: AnyRef, o2: AnyRef): Int
}