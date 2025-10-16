package data

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader

class IntegerType extends UserType {
  override def typeName = "Integer"

  override def create: AnyRef = Integer.valueOf(0)

  override def cloneObject(obj: AnyRef): AnyRef = {
    if (obj == null) return create
    Integer.valueOf(obj.asInstanceOf[Integer].intValue)
  }

  @throws[IOException]
  override def readValue(in: InputStreamReader): AnyRef = {
    val br = new BufferedReader(in)
    val s = br.readLine
    parseValue(s)
  }

  override def parseValue(ss: String): AnyRef = {
    if (ss == null || ss.trim.length == 0) return Integer.valueOf(0)
    Integer.valueOf(ss.trim.toInt)
  }

  override def getTypeComparator: Comparator = new Comparator() {
    override def compare(o1: AnyRef, o2: AnyRef): Int = {
      val a = o1.asInstanceOf[Integer]
      val b = o2.asInstanceOf[Integer]
      Integer.compare(a, b)
    }
  }

  override def serialize(obj: AnyRef): String = if (obj == null) ""
  else obj.toString

  override def deserialize(s: String): AnyRef = parseValue(s)

  override def toString: String = typeName
}