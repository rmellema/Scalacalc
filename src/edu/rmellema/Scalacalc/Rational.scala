package edu.rmellema.Scalacalc

class Rational private(n: Int, d: Int) extends Number {
  override def getValue = toReal.getValue
  override def getType  = "Rational"
  def copy(nom: Int = this.n, denom: Int = this.d) = Rational(nom, denom)
  override def unary_- : Number = Rational(-n, d)

  override def +(o: Number): Number = { (o match {
    case Integer(i)     => Rational(n + d * i, d)
    case Real(r)        => toReal + Real(r)
    case Rational(m, f) => Rational(n * f + m * d, d * f)
  }).toNumber }

  override def -(o: Number): Number = { (o match {
    case Integer(i)     => Rational(n - d * i, d)
    case Real(r)        => toReal - Real(r)
    case Rational(m, f) => Rational(n * f - m * d, d * f)
  }).toNumber }

  override def *(o: Number): Number = { (o match {
    case Integer(i)     => Rational(n * i, d).toNumber
    case Real(r)        => Real(r) * toReal
    case Rational(m, f) => Rational(n * m, d * f)
  }).toNumber }

  override def /(o: Number): Number = { (o match {
    case Integer(i)     => Rational(n, d * i)
    case Real(r)        => toReal / Real(r)
    case Rational(m, f) => Rational(n * f, d * m)
  }).toNumber}

  override def ^(o: Number): Number = { (o match {
    case Integer(i)     => Rational(math.pow(n, i).toInt, math.pow(d, i).toInt)
    case Real(r)        => toReal ^ Real(r)
    case Rational(m, f) => Rational(n * f, d * m)
  }).toNumber}

  override def %(o: Number): Number =
    sys.error("Can't take the % of a Rational")

  override def toInteger  = toReal.toInteger
  override def toReal     = Real((n + 0.0)/d)
  override def toRational = this

  def getNominal = n
  def getDenominal = d

  override def toString  =
    (if (n > d) (n/d).toString + " + " else "") +
      (n % d).toString + "/" + d.toString
}

object Rational {
  private def gcd(x: Int, y: Int): Int = {
    if (x == 0) y
    else if (x < 0)  gcd(-x, y)
    else if (y < 0) -gcd(x, -y)
    else gcd(y % x, x)
  }

  def fromInteger(n: Int) = new Rational(n, 1)

  def apply(n: Int, d: Int) = {
    val g = gcd(n, d)
    if (g == d) Integer(n/d)
    else new Rational(n/g, d/g) {}
  }

  def unapply(arg: Rational): Option[(Int, Int)] =
    Some((arg.getNominal, arg.getDenominal))
}
