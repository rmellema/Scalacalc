package edu.rmellema.Scalacalc

case class Integer(i: scala.Int) extends Number {
  override def getValue = i
  override def unary_- = Integer(-i)
  override def getType = "Integer"

  override def +(t: Number): Number = t match {
    case Integer(o) => Integer(i + o)
    case Real(r) => Real(i + r)
    case Rational(n, d) => Rational(i * d + n, d)
  }

  override def -(t: Number): Number = t match {
    case Integer(o) => Integer(i - o)
    case Real(r) => Real(i - r)
    case Rational(n, d) => Rational(i * d - n, d)
  }

  override def *(t: Number): Number = t match {
    case Integer(o) => Integer(i * o)
    case Real(r) => Real(i * r)
    case Rational(n, d) => Rational(n * i, d).toNumber
  }

  override def /(t: Number): Number = t match {
    case Integer(o) => Rational(i, o)
    case Real(r) => Real(i / r).toNumber
    case Rational(n, d) => Rational(n, d * i)
  }

  override def %(t: Number): Number = t match {
    case Integer(o) => Integer(i % o)
    case Real(r) => Real(i % r)
    case Rational(n, d) => sys.error("Trying to % a Rational")
  }

  override def ^(t: Number): Number = t match {
    case Integer(o) => Integer(math.pow(i, o).toInt)
    case Real(r) => Real(math.pow(i, r))
    case Rational(n, d) => Real(math.pow(i, n.toDouble / d))
  }

  override def toInteger = this

  override def toReal = Real(i.toDouble)

  override def toRational = Rational.fromInteger(i)

  override def toString = i.toString
}
