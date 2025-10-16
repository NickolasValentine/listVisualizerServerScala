package data

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader

class StringType extends UserType {
  override def typeName = "String"

  override def create = new String("")

  override def cloneObject(obj: AnyRef): AnyRef = {
    if (obj == null) return create
    new String(obj.asInstanceOf[String])
  }

  @throws[IOException]
  override def readValue(in: InputStreamReader): AnyRef = {
    val br = new BufferedReader(in)
    val s = br.readLine
    s
  }

  override def parseValue(ss: String): AnyRef = if (ss == null) ""
  else ss

  override def getTypeComparator: Comparator = new Comparator() {
    override def compare(o1: AnyRef, o2: AnyRef): Int = {
      val a = o1.asInstanceOf[String]
      val b = o2.asInstanceOf[String]
      a.compareTo(b)
    }
  }

  override def serialize(obj: AnyRef): String = if (obj == null) ""
  else obj.toString

  override def deserialize(s: String): AnyRef = s

  override def toString: String = typeName
}