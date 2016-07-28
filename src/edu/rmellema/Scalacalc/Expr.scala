package edu.rmellema.Scalacalc

abstract class Expr {
  type Valuation = Map[String, Value]

  def apply(v: Valuation): Value = eval(v)

  def vars: Array[String]

  def eval(v: Valuation): Value
}

case class Val(n: Value) extends Expr {
  override def toString = n.toString
  override def vars = Array()
  override def eval(v:Valuation) = n
}
case class Var(s: String) extends Expr with Value {
  override def getValue = s
  override def getType  = "Symbol"
  override def toString = s
  override def vars = Array(s)
  override def eval(v: Valuation) = v.get(s) match {
    case Some(d)=> d match {
      case n => n
    }
    case _   => sys.error("Variable '" + s + "' not found in valuation")
  }
}
object Sum {
  def apply(l: Expr, r: Expr) = Call("+", List(l, r))
}
object Sub {
  def apply(l: Expr, r:Expr) = Call("-", List(l,r))
}
object Mul {
  def apply(l: Expr, r:Expr) = Call("*", List(l, r))
}
object Div {
  def apply(l: Expr, r:Expr) = Call("/", List(l, r))
}
object Pow {
  def apply(l: Expr, r: Expr) = Call("^", List(l,r))
}
object Mod {
  def apply(l: Expr, r:Expr) = Call("%", List(l,r))
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
        case f: Function => f.call(f.prepArgs(v, a:_*):_*)
        case _ => sys.error("Trying to call a " + d.getType)
      }
      case _       => sys.error("Variable '" + n + "' not found in valuation")
    }
  }
}

