package edu.rmellema.Scalacalc
class Calculator {
  private var v = Map("e" -> Val(Real(math.E)), "pi" -> Val(Real(math.Pi)))

  def evaluate(ln: String): Number = {
    val res = Parser parse ln match {
      case Ass(n, e) =>
        val r = e(v)
        v = v + ((n.n, Val(r)))
        r
      case e => e(v)
    }
    res
  }

}
