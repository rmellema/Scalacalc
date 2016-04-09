package edu.rmellema.Scalacalc
class Calculator {
  private var v: Map[String, Value] = Map("e" -> Real(math.E), "pi" -> Real(math.Pi))

  def evaluate(ln: String): Value = {
    val res: Value = Parser parse ln match {
      case Ass(n, e) =>
        if (n.a.isEmpty) {
          val r = e(v)
          v = v + ((n.n, r))
          r
        } else {
          val func: Function = new ExprFunc(n.a.map(_.toString), e)
          v = v + ((n.n, func))
          func
        }
      case e => {
        e(v)
      }
    }
    res
  }

}
