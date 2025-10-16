package data

object Fraction {
  private def gcd(a: Long, b: Long): Long = {
    if (a == 0) b
    else if (b == 0) a
    else {
      var x = a
      var y = b
      while (y != 0) {
        val t = y
        y = x % y
        x = t
      }
      Math.abs(x)
    }
  }
}

class Fraction(var whole: Long, var num: Long, var den: Long) {
  normalize()

  // Вспомогательный конструктор
  def this() = this(0, 0, 1)

  private def normalize(): Unit = {
    if (den == 0) den = 1
    if (den < 0) {
      den = -den
      num = -num
    }
    if (num < 0 && whole > 0) {
      whole -= 1
      num = den - ((-num) % den)
    } else if (num >= den) {
      val add = num / den
      whole += add
      num = num % den
    }
    if (whole < 0 && num > 0) {
      num = -num
    }
    val g = Fraction.gcd(Math.abs(num), Math.abs(den))
    if (g != 0 && g != 1) {
      num /= g
      den /= g
    }
  }

  def toDouble: Double = whole.toDouble + num.toDouble / den.toDouble

  override def toString: String = {
    if (num == 0) {
      whole.toString
    } else if (whole == 0) {
      s"$num/$den"
    } else {
      s"$whole ${Math.abs(num)}/$den"
    }
  }
}