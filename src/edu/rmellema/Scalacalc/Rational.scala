package edu.rmellema.Scalacalc

abstract case class Rational private(n: Int, d: Int) extends Number {
  def copy(nom: Int = this.n, denom: Int = this.d) = Rational(nom, denom)
  override def unary_- : Number = Rational(-n, d)

  override def +(o: Number): Number = o match {
    case Integer(i)     => Rational(n + d * i, d)
    case Real(r)        => toReal + Real(r)
    case Rational(m, f) => Rational(n * f + m * d, d * f)
  }

  override def -(o: Number): Number = o match {
    case Integer(i)     => Rational(n - d * i, d)
    case Real(r)        => toReal - Real(r)
    case Rational(m, f) => Rational(n * f - m * d, d * f)
  }

  override def *(o: Number): Number =  o match {
    case Integer(i)     => Rational(n * i, d).toNumber
    case Real(r)        => Real(r) * toReal
    case Rational(m, f) => Rational(n * m, d * f)
  }

  override def /(o: Number): Number = o match {
    case Integer(i)     => Rational(n, d * i)
    case Real(r)        => toReal / Real(r)
    case Rational(m, f) => Rational(n * f, d * m)
  }

  override def ^(o: Number): Number = o match {
    case Integer(i)     => Rational(math.pow(n, i).toInt, math.pow(d, i).toInt)
    case Real(r)        => toReal ^ Real(r)
    case Rational(m, f) => Rational(n * f, d * m)
  }

  override def %(o: Number): Number =
    sys.error("Can't take the % of a Rational")

  override def toInteger  = toReal.toInteger
  override def toReal     = Real(n/d)
  override def toRational = this

  override def toString  = n.toString + "/" + d.toString
}

object Rational {
  private def gcd(x: Int, y: Int): Int = {
    if (x == 0) y
    else if (x < 0)  gcd(-x, y)
    else if (y < 0) -gcd(x, -y)
    else gcd(y % x, x)
  }

  def apply(n: Int, d: Int) = {
    val g = gcd(n, d)
    new Rational(n/g, d/g) {}
  }
}
