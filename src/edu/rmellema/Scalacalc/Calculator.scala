package edu.rmellema.Scalacalc
class Calculator {
  private var v: Map[String, Value] =
    Map("e"   -> Real(math.E),
        "pi"  -> Real(math.Pi),
        "nil" -> Nil,
        "abs" -> new BuiltinFunc(StdLib.abs),
        "ceil" -> new BuiltinFunc(StdLib.ceil),
        "floor" -> new BuiltinFunc(StdLib.floor),
        "max" -> new BuiltinFunc(StdLib.max),
        "min" -> new BuiltinFunc(StdLib.min),
        "rint" -> new BuiltinFunc(StdLib.rint),
        "round" -> new BuiltinFunc(StdLib.round),
        "acos" -> new BuiltinFunc(StdLib.acos),
        "asin" -> new BuiltinFunc(StdLib.asin),
        "atan" -> new BuiltinFunc(StdLib.atan),
        "cos" -> new BuiltinFunc(StdLib.cos),
        "cosh" -> new BuiltinFunc(StdLib.cosh),
        "sin" -> new BuiltinFunc(StdLib.sin),
        "sinh" -> new BuiltinFunc(StdLib.sinh),
        "tan" -> new BuiltinFunc(StdLib.tan),
        "tanh" -> new BuiltinFunc(StdLib.tanh),
        "toDegrees" -> new BuiltinFunc(StdLib.toDegrees),
        "toRadians" -> new BuiltinFunc(StdLib.toRadians),
        "log" -> new BuiltinFunc(StdLib.log))

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
