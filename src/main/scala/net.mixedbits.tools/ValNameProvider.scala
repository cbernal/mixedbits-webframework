package net.mixedbits.tools

import scala.reflect.Manifest

class ValNameFinder(obj:AnyRef){
  
  import collection.mutable._
  
  lazy val valLikeMethods = obj.getClass.getDeclaredMethods.filter(_.getParameterTypes.length == 0)
  
  class ValCache(classRef:Class[_]){
    
    lazy val map =
              valLikeMethods.filter(_.getReturnType == classRef)
                .map(method => Pair(method.getName,method.invoke(obj)))
                
    def apply(item:AnyRef) = 
      map.filter{_._2 eq item}.map{_._1}.firstOption
  }
  
  val valMap = Map[Class[_],ValCache]()
            
  def apply[T <: AnyRef](item:T) = {
    val itemClass = item.getClass
    if(!valMap.contains(itemClass))
      valMap(itemClass) = new ValCache(itemClass)
    valMap(itemClass)(item)
  }
}

trait ValNameProvider{
  implicit protected val valNameFinder = new ValNameFinder(this)
}

/*
class SomeType(value:String)(implicit nameFinder:ValNameFinder){
  lazy val name = nameFinder(this)
  
  override def toString():String = name+": "+value
}

case class AnotherType(value:String,other:Int)(implicit nameFinder:ValNameFinder){
  lazy val name = nameFinder(this)
}


object ValNameTest extends ValNameProvider{
  
  def someType(value:String) = new SomeType(value)

  val a = someType("a value")
  val b = someType("b value")
  val c = new SomeType("c value")
  
  val x = AnotherType("x",1)
  val y = new AnotherType("y",2)
  val z = AnotherType("z",3)
  
  
  def test = {
    println(a)
    println(b)
    println(c)
    
    println(x)
    println(y)
    println(z)
    
    println(x.name)
    println(y.name)
    println(z.name)
    
    println(someType("no name 1"))
    println(new SomeType("no name 2"))
    println(AnotherType("no name 3",4).name)
  }
  
}
*/
