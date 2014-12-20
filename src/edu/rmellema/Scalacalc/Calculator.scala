package edu.rmellema.Scalacalc
class Calculator {
  private var v: Map[String, Number] = Map("e" -> Real(math.E), "pi" -> Real(math.Pi))

  def evaluate(ln: String): Number = {
    val res = Parser parse ln match {
      case Ass(n, e) =>
        val r = e(v)
        v = v + ((n, r))
        r
      case e => e(v)
    }
    res
  }

}
