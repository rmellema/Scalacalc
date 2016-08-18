package edu.rmellema.Scalacalc
class Calculator {
  private var v: Valuation = Valuation.fromMap(
    Map("e"   -> Real(math.E),
        "pi"  -> Real(math.Pi),
        "nil" -> Nil,
        "abs" -> new BuiltinFunc(Var("abs"), List[Expr](Var("x")), StdLib.abs),
        "ceil" -> new BuiltinFunc(Var("ceil"), List[Expr](Var("x")), StdLib.ceil),
        "floor" -> new BuiltinFunc(Var("floor"), List[Expr](Var("x")), StdLib.floor),
        "max" -> new BuiltinFunc(Var("max"), List[Expr](Var("x"), Var("y")), StdLib.max),
        "min" -> new BuiltinFunc(Var("min"), List[Expr](Var("x"), Var("y")), StdLib.min),
        "rint" -> new BuiltinFunc(Var("rint"), List[Expr](Var("x")), StdLib.rint),
        "round" -> new BuiltinFunc(Var("round"), List[Expr](Var("x")), StdLib.round),
        "acos" -> new BuiltinFunc(Var("acos"), List[Expr](Var("x")), StdLib.acos),
        "asin" -> new BuiltinFunc(Var("asin"), List[Expr](Var("x")), StdLib.asin),
        "atan" -> new BuiltinFunc(Var("atan"), List[Expr](Var("x")), StdLib.atan),
        "cos" -> new BuiltinFunc(Var("cos"), List[Expr](Var("x")), StdLib.cos),
        "cosh" -> new BuiltinFunc(Var("cosh"), List[Expr](Var("x")), StdLib.cosh),
        "sin" -> new BuiltinFunc(Var("sin"), List[Expr](Var("x")), StdLib.sin),
        "sinh" -> new BuiltinFunc(Var("sinh"), List[Expr](Var("x")), StdLib.sinh),
        "tan" -> new BuiltinFunc(Var("tan"), List[Expr](Var("x")), StdLib.tan),
        "tanh" -> new BuiltinFunc(Var("tanh"), List[Expr](Var("x")), StdLib.tanh),
        "toDegrees" -> new BuiltinFunc(Var("toDegrees"), List[Expr](Var("x")), StdLib.toDegrees),
        "toRadians" -> new BuiltinFunc(Var("toRadians"), List[Expr](Var("x")), StdLib.toRadians),
        "log" -> new BuiltinFunc(Var("log"), List[Expr](Var("b"), Var("x")), StdLib.log),
        "+" -> new BuiltinFunc(Var("+"), List[Expr](Var("x"), Var("y")), StdLib.add),
        "-" -> new BuiltinFunc(Var("-"), List[Expr](Var("x"), Var("y")), StdLib.sub),
        "*" -> new BuiltinFunc(Var("*"), List[Expr](Var("x"), Var("y")), StdLib.mul),
        "/" -> new BuiltinFunc(Var("/"), List[Expr](Var("x"), Var("y")), StdLib.div),
        "^" -> new BuiltinFunc(Var("^"), List[Expr](Var("x"), Var("y")), StdLib.pow),
        "%" -> new BuiltinFunc(Var("%"), List[Expr](Var("x"), Var("y")), StdLib.mod) ))

  def evaluate(ln: String): Value = {
    val res: Value = Parser parse ln match {
      case Ass(n, e) =>
        if (n.a.isEmpty) {
          val r = e(v)
          v.set(Var(n.n), r)
          r
        } else {
          val func: Function = new ExprFunc(Var(n.n), n.a, e, v.filterKeys(e.vars.contains(_)))
          v.set(n, func)
          func
        }
      case e => {
        e(v)
      }
    }
    res
  }

}
