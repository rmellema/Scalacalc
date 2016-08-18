package edu.rmellema.Scalacalc

/**
  * Created by rene on 28/07/16.
  */
class Valuation(val parent: Option[Valuation]) {
  def this() = this(None)
  var symbolTable: Map[String, Value] = Map[String, Value]()

  def get(sym: String): Option[Value] = {
    symbolTable.get(sym) match {
      case Some(v) => Some(v)
      case _ => parent match {
        case Some(p) => p.get(sym)
        case _ => None
      }
    }
  }

  def get(sym: Var): Option[Value] = get(sym.s)

  def get(call: Call): Option[Value] = {
    get(call.n) match {
      case Some(f) => f match {
        case funcs: FunctionContainer => {
          val params = call.a.length
          funcs.funcs.find(_.getArgs().length == params)
        }
        case _ => Some(f)
      }
      case None => None
    }
  }

  def set(sym: Var, v: Value): Unit = {
    get(sym) match {
      case Some(_) => sys.error("Trying to redefine " + sym.toString)
      case None => symbolTable = symbolTable + ((sym.toString, v))
    }
  }

  def set(call: Call, func: Function): Unit = {
    get(call.n) match {
      case Some(f: FunctionContainer) => f.addFunction(func)
      case Some(f: Function) => {
        symbolTable = symbolTable + ((call.n, new FunctionContainer(Var(call.n), f, func)))
      }
      case _ => set(Var(call.n), func)
    }
  }

  def keys = symbolTable.keys

  def merge(other: Valuation): Valuation = {
    val ret = new Valuation(Some(this))
    val myKeys = symbolTable.keySet
    for (k <- other.keys if !(myKeys.contains(k))) {
      ret.set(Var(k), other.get(k).get)
    }
    ret
  }

  def filterKeys(p: (String) => Boolean): Valuation = {
    val ret = new Valuation()
    for (k <- keys.filter(p))
      ret.set(Var(k), get(k).get)
    ret
  }

}

object Valuation {
  def fromMap(m: Map[String, Value]): Valuation = {
    var valuation = new Valuation()
    for (k <- m.keys) {
      valuation.set(Var(k), m.get(k).get)
    }
    valuation
  }
}
