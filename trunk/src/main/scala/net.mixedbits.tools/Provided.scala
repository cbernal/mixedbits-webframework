package net.mixedbits.tools

import sys.error
import scala.util._
import BlockStatements._

trait Provided[T]{
  
  def default:T
  private lazy val cachedDefault = default
  def findDefault = attempt{cachedDefault}
  
  
  private lazy val dynamicValue = new DynamicVariable(findDefault)
  
  protected def notFoundHandler():T = error("no value provided")
  
  def apply():T = {
    for(value <- dynamicValue.value)
      return value
    
    notFoundHandler()
  }
  
  def apply[R](value:T)(body: =>R):R = 
    dynamicValue.withValue(Some(value)){body}
}

trait NoDefault{
  self:Provided[_] =>
  def default = error("no default provided")
}

/*
object Provided{
  def apply[T](implicit provided:Provided[T]):T = provided()
}

trait Interface1{
  def a() = println("a called")
}

object ProvidedTest1 extends Provided[Interface1]{
  def default = new Interface1{}
}

object ProvidedTest2 extends Provided[Interface1] with NoDefault

object ProvidedTest{
  
  implicit val x = ProvidedTest1
  def run(){
    
    Provided[Interface1].a()
    
    ProvidedTest1().a()
    ProvidedTest2().a()
  }
}
*/
