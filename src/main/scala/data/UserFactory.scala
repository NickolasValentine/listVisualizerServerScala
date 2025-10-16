package data

import java.util

class UserFactory {
  private val builders: util.ArrayList[UserType] = new util.ArrayList[UserType]()

  // Инициализация — происходит при создании экземпляра
  {
    builders.add(new IntegerType())
    builders.add(new DoubleType())
    builders.add(new StringType())
    builders.add(new FractionType())
  }

  // Для выпадающего списка в GUI
  def getTypeNameList: util.ArrayList[String] = {
    val names = new util.ArrayList[String]()
    val it = builders.iterator()
    while (it.hasNext) {
      names.add(it.next().typeName)
    }
    names
  }

  // Для получения прототипа по имени
  def getBuilderByName(name: String): UserType = {
    val it = builders.iterator()
    while (it.hasNext) {
      val u = it.next()
      if (u.typeName == name) {
        return u
      }
    }
    null
  }

  def getAllBuilders: util.ArrayList[UserType] = builders
}