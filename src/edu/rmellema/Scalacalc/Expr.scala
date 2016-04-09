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
case class Sum(l: Expr, r:Expr) extends Expr {
  override def toString = "(" + l.toString + " + " + r.toString + ")"
  override def vars = l.vars.union(r.vars)
  override def eval(v: Valuation) = l.eval(v) match {
    case lv: Number => r.eval(v) match {
      case rv: Number => lv + rv
      case _ => sys.error("Trying to add a number to a non number")
    }
    case lv => sys.error("Trying to add a non number: " + lv.getType)
  }
}
case class Sub(l: Expr, r:Expr) extends Expr {
  override def toString = "(" + l.toString + " - " + r.toString + ")"
  override def vars = l.vars.union(r.vars)
  override def eval(v: Valuation) = l.eval(v) match {
    case lv: Number => r.eval(v) match {
      case rv: Number => lv - rv
      case _ => sys.error("Trying to subtract a number to a non number")
    }
    case _ => sys.error("Trying to subtract a non number")
  }
}
case class Mul(l: Expr, r:Expr) extends Expr {
  override def toString = "(" + l.toString + " * " + r.toString + ")"
  override def vars = l.vars.union(r.vars)
  override def eval(v: Valuation) = l.eval(v) match {
    case lv: Number => r.eval(v) match {
      case rv: Number => lv * rv
      case _ => sys.error("Trying to multiply a number to a non number")
    }
    case _ => sys.error("Trying to multiply a non number")
  }
}
case class Div(l: Expr, r:Expr) extends Expr {
  override def toString = "(" + l.toString + " / " + r.toString + ")"
  override def vars = l.vars.union(r.vars)
  override def eval(v: Valuation) = l.eval(v) match {
    case lv: Number => r.eval(v) match {
      case rv: Number => lv / rv
      case _ => sys.error("Trying to divide a number to a non number")
    }
    case _ => sys.error("Trying to divide a non number")
  }
}
case class Pow(l: Expr, r: Expr) extends Expr {
  override def toString = "(" + l.toString + " ^ " + r.toString + ")"
  override def vars = l.vars.union(r.vars)
  override def eval(v: Valuation) = l.eval(v) match {
    case lv: Number => r.eval(v) match {
      case rv: Number => lv ^ rv
      case _ => sys.error("Trying to take the power of a number to a non number")
    }
    case _ => sys.error("Trying to take the power of a non number")
  }
}
case class Mod(l: Expr, r:Expr) extends Expr {
  override def toString = "(" + l.toString + " % " + r.toString + ")"
  override def vars = l.vars.union(r.vars)
  override def eval(v: Valuation) = l.eval(v) match {
    case lv: Number => r.eval(v) match {
      case rv: Number => lv % rv
      case _ => sys.error("Trying to modulo a number to a non number")
    }
    case _ => sys.error("Trying to modulo a non number: " + l.getClass.toString)
  }
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

