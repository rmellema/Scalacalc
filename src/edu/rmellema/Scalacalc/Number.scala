package edu.rmellema.Scalacalc

abstract class Number extends Value{
  override def isNumber = true
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
