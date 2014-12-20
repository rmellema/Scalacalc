package edu.rmellema.Scalacalc

abstract class Number {
  def toInteger : Integer
  def toReal    : Real
  def toNumber  : Number = this match {
    case Real(d) =>
      if (d - d.floor == 0) Integer(d.toInt)
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
    case Integer(o) => Real(i.toDouble / o).toNumber
    case Real   (r) => Real(i / r).toNumber
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
    case Real(o)    => Real(d + o).toNumber
  }
  override def -(n: Number): Number = n match {
    case Integer(i) => Real(d - i)
    case Real(o)    => Real(d - o).toNumber
  }
  override def *(n: Number): Number = n match {
    case Integer(i) => Real(d * i).toNumber
    case Real(o)    => Real(d * o).toNumber
  }
  override def /(n: Number): Number = n match {
    case Integer(i) => Real(d / i).toNumber
    case Real(o)    => Real(d / o).toNumber
  }
  override def %(n: Number): Number = n match {
    case Integer(i) => Real(d % i).toNumber
    case Real(o)    => Real(d % o).toNumber
  }
  override def ^(n: Number): Number = n match {
    case Integer(i) => Real(math.pow(d, i)).toNumber
    case Real(o)    => Real(math.pow(d, o)).toNumber
  }
  override def toInteger = Integer(d.toInt)
  override def toReal    = this

  override def toString  = d.toString
}
