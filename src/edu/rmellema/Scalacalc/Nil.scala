package edu.rmellema.Scalacalc

object Nil extends Value {
  override def getValue: Any = null
  override def getType: String = "nil"
  override def toString = "nil"
  override def isTruthy = false
}
