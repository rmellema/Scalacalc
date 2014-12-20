package edu.rmellema.Scalacalc

abstract class Number {
  def toInteger  : Integer
  def toReal     : Real
  def toRational : Rational
  def toNumber   : Number = this match {
    case Real(d) =>
      if (d - d.floor == 0) Integer(d.toInt)
      else this
    case Rational(n, d) =>
      if (d == 1 || n % d == 0) Integer(n / d)
      else this
    case _       => this
  }

  def toString : String

  def unary_- : Number
  def +(o: Number): Number
  def -(o: Number): Number
  def *(o: Number): Number
  def /(o: Number): Number
  def %(o: Number): Number
  def ^(o: Number): Number
}

case class Integer(i: scala.Int) extends Number {
  override def unary_- = Integer(-i)

  override def +(t: Number): Number = t match {
    case Integer (o)    => Integer(i + o)
    case Real    (r)    => Real(i + r)
    case Rational(n, d) => Rational(i * d + n, d)
  }
  override def -(t: Number): Number = t match {
    case Integer (o)    => Integer(i - o)
    case Real    (r)    => Real(i - r)
    case Rational(n, d) => Rational(i * d - n, d)
  }
  override def *(t: Number): Number = t match {
    case Integer(o) => Integer(i * o)
    case Real   (r) => Real(i * r)
    case Rational(n, d) => Rational(n * i, d).toNumber
  }
  override def /(t: Number): Number = t match {
    case Integer(o) => Rational(i, o)
    case Real   (r) => Real(i / r).toNumber
    case Rational(n, d) => Rational(n, d * i)
  }
  override def %(t: Number): Number = t match {
    case Integer(o) => Integer(i % o)
    case Real   (r) => Real(i % r)
    case Rational(n, d) => sys.error("Trying to % a Rational")
  }
  override def ^(t: Number): Number = t match {
    case Integer(o) => Integer(math.pow(i, o).toInt)
    case Real   (r) => Real(math.pow(i, r))
    case Rational(n, d) => Real(math.pow(i, n.toDouble / d))
  }

  override def toInteger  = this
  override def toReal     = Real(i.toDouble)
  override def toRational = Rational(i, 1)

  override def toString  = i.toString
}

case class Real(d: scala.Double) extends Number {
  override def unary_- = Real(-d)
  override def +(t: Number): Number = t match {
    case Integer(i) => Real(d + i)
    case Real(o)    => Real(d + o).toNumber
    case Rational(_, _) => this + t.toReal
  }
  override def -(t: Number): Number = t match {
    case Integer(i) => Real(d - i)
    case Real(o)    => Real(d - o).toNumber
    case Rational(_, _) => this - t.toReal
  }
  override def *(t: Number): Number = t match {
    case Integer(i) => Real(d * i).toNumber
    case Real(o)    => Real(d * o).toNumber
    case Rational(_, _) => this * t.toReal
  }
  override def /(t: Number): Number = t match {
    case Integer(i) => Real(d / i).toNumber
    case Real(o)    => Real(d / o).toNumber
    case Rational(_, _) => this / t.toReal
  }
  override def %(t: Number): Number = t match {
    case Integer(i) => Real(d % i).toNumber
    case Real(o)    => Real(d % o).toNumber
    case Rational(_, _) => this % t.toReal
  }
  override def ^(t: Number): Number = t match {
    case Integer(i) => Real(math.pow(d, i)).toNumber
    case Real(o)    => Real(math.pow(d, o)).toNumber
    case Rational(_, _) => this ^ t.toReal
  }
  override def toInteger  = Integer(d.toInt)
  override def toReal     = this
  override def toRational = sys.error("Implement this!")

  override def toString  = d.toString
}

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
