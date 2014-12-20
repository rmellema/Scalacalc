abstract class Expr {
  type Valuation = Map[String, Double]

  def apply(v: Valuation): Double = eval(v)

  def vars: Array[String]

  def eval(v: Valuation): Double
}

case class Val(d: Double) extends Expr {
  override def toString = d.toString
  override def vars = Array()
  override def eval(v:Valuation) = d
}
case class Var(s: String) extends Expr {
  override def toString = s
  override def vars = Array(s)
  override def eval(v: Valuation) = v.get(s) match {
    case Some(d) => d
    case _       => sys.error("Variable '" + s + "' not found in valuation")
  }
}
case class Sum(l: Expr, r:Expr) extends Expr {
  override def toString = "(" + l.toString + " + " + r.toString + ")"
  override def vars = l.vars.union(r.vars)
  override def eval(v: Valuation) = l.eval(v) + r.eval(v)
}
case class Sub(l: Expr, r:Expr) extends Expr {
  override def toString = "(" + l.toString + " - " + r.toString + ")"
  override def vars = l.vars.union(r.vars)
  override def eval(v: Valuation) = l.eval(v) - r.eval(v)
}
case class Mul(l: Expr, r:Expr) extends Expr {
  override def toString = "(" + l.toString + " * " + r.toString + ")"
  override def vars = l.vars.union(r.vars)
  override def eval(v: Valuation) = l.eval(v) * r.eval(v)
}
case class Div(l: Expr, r:Expr) extends Expr {
  override def toString = "(" + l.toString + " / " + r.toString + ")"
  override def vars = l.vars.union(r.vars)
  override def eval(v: Valuation) = l.eval(v) / r.eval(v)
}
case class Pow(l: Expr, r: Expr) extends Expr {
  override def toString = "(" + l.toString + " ^ " + r.toString + ")"
  override def vars = l.vars.union(r.vars)
  override def eval(v: Valuation) = math.pow(l.eval(v), r.eval(v))
}
case class Mod(l: Expr, r:Expr) extends Expr {
  override def toString = "(" + l.toString + " % " + r.toString + ")"
  override def vars = l.vars.union(r.vars)
  override def eval(v: Valuation) = l.eval(v) % r.eval(v)
}
case class Ass(n: String, r:Expr) extends Expr {
  override def toString = "(" + n + " = " + r.toString+")"
  override def vars = n +: r.vars
  override def eval(v: Valuation) = r.eval(v)
}
