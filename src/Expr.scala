type Valuation = Map[String, Double]

abstract class Expr

def vars(e :Expr) :Array[String] = e match {
  case Val(d) => Array()
  case Var(s) => Array(s)
  case Sum(l, r) => vars(l).union(vars(r))
  case Sub(l, r) => vars(l).union(vars(r))
  case Mul(l, r) => vars(l).union(vars(r))
  case Div(l, r) => vars(l).union(vars(r))
  case Mod(l, r) => vars(l).union(vars(r))
}

def eval (e:Expr, v :Valuation) :Double = e match {
  case Val(d) => d
  case Var(s) => v.get(s) match {
    case Some(r) => r
    case _       => sys.error("Variable " + s + "Not found!")
  }
  case Sum(l, r) => eval(l, v) + eval(r, v)
  case Sub(l, r) => eval(l, v) - eval(r, v)
  case Mul(l, r) => eval(l, v) * eval(r, v)
  case Div(l, r) => eval(l, v) / eval(r, v)
  case Mod(l, r) => eval(l, v) % eval(r, v)
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
