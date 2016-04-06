package edu.rmellema.Scalacalc

abstract class Expr {
  type Valuation = Map[String, Expr]

  def apply(v: Valuation): Number = eval(v)

  def vars: Array[String]

  def eval(v: Valuation): Number
}

case class Val(n: Number) extends Expr {
  override def toString = n.toString
  override def vars = Array()
  override def eval(v:Valuation) = n
}
case class Var(s: String) extends Expr {
  override def toString = s
  override def vars = Array(s)
  override def eval(v: Valuation) = v.get(s) match {
    case Some(d)=> d match {
      case Val(n) => n
    }
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
  override def eval(v: Valuation) = l.eval(v) ^ r.eval(v)
}
case class Mod(l: Expr, r:Expr) extends Expr {
  override def toString = "(" + l.toString + " % " + r.toString + ")"
  override def vars = l.vars.union(r.vars)
  override def eval(v: Valuation) = l.eval(v) % r.eval(v)
}
case class Ass(n: Call, r: Expr) extends Expr {
  override def toString = "(" + n + " = " + r.toString+")"
  override def vars = n.vars.union(r.vars)
  override def eval(v: Valuation) = r(v)
}
case class Call(n: String, a: List[Expr]) extends Expr {
  override def toString = n +  a.mkString("(", ", ", ")")
  override def vars: Array[String] = Array.empty[String]

  override def eval(v: Valuation) = {
    v.get(n) match {
      case Some(d) => d match {
        case Func(p, e) => e(v ++ p.zip(a.map((x: Expr) => Val(x.eval(v)))))
      }
      case _       => sys.error("Variable '" + n + "' not found in valuation")
    }
  }
}
case class Func(p: List[String], e: Expr) extends Expr {
  override def toString = e.toString
  override def vars: Array[String] = p.union(e.vars).toArray

  override def eval(v: Valuation): Number = {
    println(v)
    e(v)
  }
}
