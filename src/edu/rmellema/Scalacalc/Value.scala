package edu.rmellema.Scalacalc

/**
  * Created by rene on 6/04/16.
  */
trait Value {
  def isNumber   = false
  def isSymbol   = false
  def isCallable = false
  def isTruthy   = true
  def toString: String
  def getValue: Any
  def getType : String
}
