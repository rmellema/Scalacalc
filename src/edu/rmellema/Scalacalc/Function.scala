package edu.rmellema.Scalacalc

trait Function extends Value {
  override def getType  = "Function"

  def getName(): Var
  def getArgs(): List[Expr]
  def call(v: Value*): Value
  def prepArgs(v: Valuation, es: Expr*): Seq[Value] = es.map(_.eval(v))
}

class ExprFunc(val sym: Var, val args: List[Expr], val body: Expr, val closure: Valuation) extends Function {
  val param = args.map(_.toString)

  override def getName(): Var = sym
  override def getArgs(): List[Expr] = args
  override def getValue = (param, body)
  override def toString = param.mkString("(", ", ", ")") + " = " + body.toString

  override def call(vs: Value*): Value = {
    body.eval(Valuation.fromMap(param.zip(vs).toMap[String, Value]).merge(closure))
  }
}

class BuiltinFunc(val sym: Var, val args:List[Expr], val func: (Value*) => Value) extends Function {
  override def getName(): Var = sym
  override def getArgs(): List[Expr] = args
  override def getValue = func
  override def toString = "Builtin: " + func.toString()

  override def call(v: Value*) = func(v:_*)
}

class FunctionContainer(private val name: Var, var funcs: List[Function]) extends Function {
  def this(sym: Var, fs: Function*) = {
    this(sym, List[Function]())
    for (f <- fs) {
      addFunction(f)
    }
  }

  def addFunction(func: Function) = {
    val arglen = func.getArgs().length
    if (funcs.map(_.getArgs().length).contains(arglen)) {
      sys.error("Trying to redefine function " + name.s + " with arity " + arglen)
    } else {
      funcs ::= func
    }
  }

  override def getName(): Var = name
  override def getArgs(): List[Expr] = funcs.head.getArgs()

  override def call(v: Value*): Value = {
    val params = v.length
    val candidate = funcs.filter(_.getArgs().length == params)
    if (candidate.isEmpty) {
      sys.error("Trying to call " + name.s + " with wrong number of arguments: " + params)
    } else {
      candidate.head.call(v:_*)
    }
  }

  override def getValue: Any = funcs.map(_.getValue)
}