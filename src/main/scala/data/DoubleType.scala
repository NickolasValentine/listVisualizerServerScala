package data

import java.io.{BufferedReader, IOException, InputStreamReader}
import java.lang.Double

class DoubleType extends UserType {
  override def typeName: String = "Double"

  override def create: AnyRef = Double.valueOf(0.0)

  override def cloneObject(obj: AnyRef): AnyRef = {
    if (obj == null) create
    else Double.valueOf(obj.asInstanceOf[Double])
  }

  @throws[IOException]
  override def readValue(in: InputStreamReader): AnyRef = {
    val br = new BufferedReader(in)
    val s = br.readLine()
    parseValue(s)
  }

  override def parseValue(ss: String): AnyRef = {
    if (ss == null || ss.trim.isEmpty) Double.valueOf(0.0)
    else Double.valueOf(ss.trim.toDouble)
  }

  override def getTypeComparator: Comparator = new Comparator {
    override def compare(o1: AnyRef, o2: AnyRef): Int = {
      val a = o1.asInstanceOf[Double]
      val b = o2.asInstanceOf[Double]
      java.lang.Double.compare(a, b)
    }
  }

  override def serialize(obj: AnyRef): String =
    if (obj == null) "" else obj.toString

  override def deserialize(s: String): AnyRef = parseValue(s)

  override def toString: String = typeName
}