package edu.rmellema.Scalacalc

trait Function extends Value {
  override def getType  = "Function"

  def call(v: Value*): Value
  def prepArgs(v: Expr#Valuation, es: Expr*): Seq[Value] = es.map(_.eval(v))
}

class ExprFunc(param: List[String], body: Expr) extends Function {
  override def getValue = (param, body)
  override def toString = param.mkString("(", ", ", ")") + " = " + body.toString

  override def call(vs: Value*): Value =
    body.eval(param.zip(vs).toMap[String, Value])
}

