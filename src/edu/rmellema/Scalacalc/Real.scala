package edu.rmellema.Scalacalc

case class Real(d: scala.Double) extends Number {
  override def getValue = d
  override def getType  = "Real"
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
  override def toRational = sys.error("Cannot convert a real to a double")

  override def toString  = d.toString
}

