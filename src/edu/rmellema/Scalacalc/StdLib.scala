package edu.rmellema.Scalacalc

/**
  * Created by rene on 9/04/16.
  */
object StdLib {
  private def noFunc(name: String, args: Seq[Value]) = {
    sys.error("No function \'" + name + "\' found for signature " +
      args.map(_.getType).mkString("(", ", ", ")"))
  }

  // Abs and rounding functions
  def abs(args: Value*): Value = args match {
    case Seq(x: Integer)  => Integer(math.abs(x.getValue))
    case Seq(x: Real)     => Real(math.abs(x.getValue))
    case Seq(x: Rational) => Rational(math.abs(x.getNominal),
                                      math.abs(x.getDenominal))
    case Seq(x: Value)    => noFunc("abs", args)
  }

  def ceil(args: Value*): Value = args match {
    case Seq(x: Number) => Real(math.ceil(x.toReal.getValue)).toInteger
    case _              => noFunc("ceil", args)
  }

  def floor(args:Value*): Value = args match {
    case Seq(x: Number) => Real(math.floor(x.toReal.getValue)).toInteger
    case _              => noFunc("floor", args)
  }

  def max(args: Value*): Value = args match {
    case Seq(x: Number, y: Number) =>
      Real(math.max(x.toReal.getValue, y.toReal.getValue)).toNumber
    case _ => noFunc("max", args)
  }

  def min(args: Value*): Value = args match {
    case Seq(x: Number, y: Number) =>
      Real(math.min(x.toReal.getValue, y.toReal.getValue)).toNumber
    case _ => noFunc("min", args)
  }

  def rint(args:Value*): Value = args match {
    case Seq(x: Number) => Real(math.rint(x.toReal.getValue)).toNumber
    case _ => noFunc("rint", args)
  }

  def round(args: Value*): Value = args match {
    case Seq(x: Number) => Real(math.round(x.toReal.getValue)).toNumber
    case _ => noFunc("round", args)
  }

  // mathematical functions
  def log(args: Value*): Value = args match {
    case Seq(e: Number, x: Number) =>
      Real (math.log(x.toReal.getValue) /
        math.log(e.toReal.getValue)).toNumber
    case Seq(x: Number) =>
      Real (math.log (x.toReal.getValue) ).toNumber
    case _  => noFunc("log", args)
  }

  def acos(args: Value*): Value = args match {
    case Seq(x: Number) => Real(math.acos(x.toReal.getValue)).toNumber
    case _ => noFunc("acos", args)
  }

  def asin(args: Value*): Value = args match {
    case Seq(x: Number) => Real(math.asin(x.toReal.getValue)).toNumber
    case _ => noFunc("asin", args)
  }

  def atan(args: Value*): Value = args match {
    case Seq(x: Number) => Real(math.atan(x.toReal.getValue)).toNumber
    case _ => noFunc("atan", args)
  }

  def cos(args: Value*): Value = args match {
    case Seq(x: Number) => Real(math.cos(x.toReal.getValue)).toNumber
    case _ => noFunc("cos", args)
  }

  def cosh(args: Value*): Value = args match {
    case Seq(x: Number) => Real(math.cosh(x.toReal.getValue)).toNumber
    case _ => noFunc("cosh", args)
  }

  def sin(args: Value*): Value = args match {
    case Seq(x: Number) => Real(math.sin(x.toReal.getValue)).toNumber
    case _ => noFunc("sin", args)
  }

  def sinh(args: Value*): Value = args match {
    case Seq(x: Number) => Real(math.sinh(x.toReal.getValue)).toNumber
    case _ => noFunc("sinh", args)
  }

  def tan(args: Value*): Value = args match {
    case Seq(x: Number) => Real(math.tan(x.toReal.getValue)).toNumber
    case _ => noFunc("tan", args)
  }

  def tanh(args: Value*): Value = args match {
    case Seq(x: Number) => Real(math.tanh(x.toReal.getValue)).toNumber
    case _ => noFunc("tanh", args)
  }

  def toDegrees(args: Value*): Value = args match {
    case Seq(x: Number) => Real(math.toDegrees(x.toReal.getValue)).toNumber
    case _ => noFunc("toDegrees", args)
  }

  def toRadians(args: Value*): Value = args match {
    case Seq(x: Number) => Real(math.toRadians(x.toReal.getValue)).toNumber
    case _ => noFunc("toRadians", args)
  }


}
