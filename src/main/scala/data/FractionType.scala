package data

import java.io.{BufferedReader, IOException, InputStreamReader}
import java.lang.{Double => JDouble, Long => JLong}

class FractionType extends UserType {
  override def typeName: String = "Fraction"

  override def create: AnyRef = new Fraction(0, 0, 1)

  override def cloneObject(obj: AnyRef): AnyRef = {
    if (obj == null) create
    else {
      val f = obj.asInstanceOf[Fraction]
      new Fraction(f.whole, f.num, f.den)
    }
  }

  @throws[IOException]
  override def readValue(in: InputStreamReader): AnyRef = {
    val br = new BufferedReader(in)
    val s = br.readLine()
    parseValue(s)
  }

  override def parseValue(ss: String): AnyRef = {
    if (ss == null || ss.trim.isEmpty) return new Fraction()
    val s = ss.trim
    try {
      if (s.contains(" ")) {
        val parts = s.split("\\s+")
        val w = JLong.parseLong(parts(0))
        val fr = parts(1)
        val nd = fr.split("/")
        val n = JLong.parseLong(nd(0))
        val d = JLong.parseLong(nd(1))
        new Fraction(w, n, d)
      } else if (s.contains("/")) {
        val nd = s.split("/")
        val n = JLong.parseLong(nd(0))
        val d = JLong.parseLong(nd(1))
        new Fraction(0, n, d)
      } else {
        val w = JLong.parseLong(s)
        new Fraction(w, 0, 1)
      }
    } catch {
      case _: Exception =>
        new Fraction()
    }
  }

  override def getTypeComparator: Comparator = new Comparator {
    override def compare(o1: AnyRef, o2: AnyRef): Int = {
      val a = o1.asInstanceOf[Fraction]
      val b = o2.asInstanceOf[Fraction]

      val anum = a.whole * a.den + a.num
      val aden = a.den
      val bnum = b.whole * b.den + b.num
      val bden = b.den

      try {
        val left = anum * bden
        val right = bnum * aden
        JLong.compare(left, right)
      } catch {
        case _: ArithmeticException =>
          val da = a.toDouble
          val db = b.toDouble
          JDouble.compare(da, db)
      }
    }
  }

  override def serialize(obj: AnyRef): String =
    if (obj == null) "" else obj.toString

  override def deserialize(s: String): AnyRef = parseValue(s)

  override def toString: String = typeName
}