abstract class Expr{
  type Valuation = Map[String, Double]
  def vars() :Array[String] = this match {
    case Val(d) => Array()
    case Var(s) => Array(s)
    case Sum(l, r) => l.vars().union(r.vars())
    case Sub(l, r) => l.vars().union(r.vars())
    case Mul(l, r) => l.vars().union(r.vars())
    case Div(l, r) => l.vars().union(r.vars())
    case Mod(l, r) => l.vars().union(r.vars())
  }

  def eval (v :Valuation) :Double = this match {
    case Val(d) => d
    case Var(s) => v.get(s) match {
      case Some(r) => r
      case _       => sys.error("Variable '" + s + "' not found!")
    }
    case Sum(l, r) => l.eval(v) + r.eval(v)
    case Sub(l, r) => l.eval(v) - r.eval(v)
    case Mul(l, r) => l.eval(v) * r.eval(v)
    case Div(l, r) => l.eval(v) / r.eval(v)
    case Mod(l, r) => l.eval(v) % r.eval(v)
  }
}

case class Val(d: Double) extends Expr {
   override def toString = d.toString
}
case class Var(s: String) extends Expr {
  override def toString = s
}
case class Sum(l: Expr, r:Expr) extends Expr {
  override def toString = "(" + r.toString + " + " + l.toString + ")"
}
case class Sub(l: Expr, r:Expr) extends Expr {
  override def toString = "(" + r.toString + " - " + l.toString + ")"
}
case class Mul(l: Expr, r:Expr) extends Expr {
  override def toString = "(" + r.toString + " * " + l.toString + ")"
}
case class Div(l: Expr, r:Expr) extends Expr {
  override def toString = "(" + r.toString + " / " + l.toString + ")"
}
case class Mod(l: Expr, r:Expr) extends Expr {
  override def toString = "(" + r.toString + " % " + l.toString + ")"
}
