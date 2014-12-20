package edu.rmellema.Scalacalc

abstract class Number {
  def toInteger : Integer
  def toReal    : Real

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

  override def +(n: Number): Number = n match {
    case Integer(o) => Integer(i + o)
    case Real   (r) => Real(i + r)
  }
  override def -(n: Number): Number = n match {
    case Integer(o) => Integer(i - o)
    case Real   (r) => Real(i - r)
  }
  override def *(n: Number): Number = n match {
    case Integer(o) => Integer(i * o)
    case Real   (r) => Real(i * r)
  }
  override def /(n: Number): Number = n match {
    case Integer(o) => Integer(i / o)
    case Real   (r) => Real(i / r)
  }
  override def %(n: Number): Number = n match {
    case Integer(o) => Integer(i % o)
    case Real   (r) => Real(i % r)
  }
  override def ^(n: Number): Number = n match {
    case Integer(o) => Integer(i ^ o)
    case Real   (r) => Real(math.pow(i, r))
  }

  override def toInteger = this
  override def toReal    = Real(i.toDouble)

  override def toString  = i.toString
}

case class Real(d: scala.Double) extends Number {
  override def unary_- = Real(-d)
  override def +(n: Number): Number = n match {
    case Integer(i) => Real(d + i)
    case Real(o)    => Real(d + o)
  }
  override def -(n: Number): Number = n match {
    case Integer(i) => Real(d - i)
    case Real(o)    => Real(d - o)
  }
  override def *(n: Number): Number = n match {
    case Integer(i) => Real(d * i)
    case Real(o)    => Real(d * o)
  }
  override def /(n: Number): Number = n match {
    case Integer(i) => Real(d / i)
    case Real(o)    => Real(d / o)
  }
  override def %(n: Number): Number = n match {
    case Integer(i) => Real(d % i)
    case Real(o)    => Real(d % o)
  }
  override def ^(n: Number): Number = n match {
    case Integer(i) => Real(math.pow(d, i))
    case Real(o)    => Real(math.pow(d, o))
  }
  override def toInteger = Integer(d.toInt)
  override def toReal    = this

  override def toString  = d.toString
}
