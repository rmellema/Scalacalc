package edu.rmellema.Scalacalc

/**
  * Created by rene on 6/04/16.
  */
trait Value {
  def isNumber = false
  def isSymbol = false
  def isCallable = false
  def toString: String
  def getValue: Any
}
