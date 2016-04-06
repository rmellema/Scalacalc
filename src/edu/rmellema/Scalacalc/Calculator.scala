package edu.rmellema.Scalacalc
class Calculator {
  private var v: Map[String, Expr] = Map("e" -> Val(Real(math.E)), "pi" -> Val(Real(math.Pi)))

  def evaluate(ln: String): Expr = {
    val res: Expr = Parser parse ln match {
      case Ass(n, e) =>
        if (n.a.isEmpty) {
          val r = e(v)
          v = v + ((n.n, Val(r)))
          Val(r)
        } else {
          val func: Func = Func(n.a.map(_.toString), e)
          v = v + ((n.n, func))
          func
        }
      case e => {
        Val(e(v))
      }
    }
    res
  }

}
