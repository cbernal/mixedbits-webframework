package net.mixedbits.json

import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._

import com.mongodb._
import com.mongodb.util._

class JsArray[T](val list:BasicDBList) extends Seq[T]{
  def this() = this(new BasicDBList)
  
  def length = list.size
  def iterator = new Iterator[T]{
    private var currentIndex = 0 
    def next():T = {currentIndex+=1;apply(currentIndex-1)}
    def hasNext():Boolean = currentIndex < list.size
  }
  def apply(i:Int):T = {
    val item = list.get(i)
    if(item.isInstanceOf[BasicDBList])
      new JsArray[Any](item.asInstanceOf[BasicDBList]).asInstanceOf[T]
    else if(item.isInstanceOf[BasicDBObject])
      new JsObject(item.asInstanceOf[BasicDBObject]).asInstanceOf[T]
    else
      item.asInstanceOf[T]
  }
  def update(i:Int,value:T) = list.put(i,value)
  def add(firstValue:T,values:T*) = {
    this += firstValue
    
    addAll(values)
  }
  
  def addAll(values:Seq[T]) = this ++= values
  
  def ++= (values:Seq[T]) = {
    for(value <- values)
      this += value
    
    this
  }
  
  def --= (values:Seq[T]) = {
    for(value <- values)
      this -= value
    
    this
  }
  

  def +=(value:T):Boolean =
    if(value.isInstanceOf[JsObject])
      list.add(value.asInstanceOf[JsObject].obj)
    else if(value.isInstanceOf[JsArray[_]])
      list.add(value.asInstanceOf[JsArray[_]].list)
    else
      list.add(value.asInstanceOf[AnyRef])
    
  def +=(value:Option[T]):Boolean = 
    value match {
      case Some(v) => this += v
      case None => false
    }
  

  def -=(value:T){ while(list contains value){list.remove(value)} }
  
  def clear:JsArray[T] = {
    list.clear
    this
  }
  
  def toJson():String = list.toString
}

class JsPhantomArray[T](obj:JsObject,property:JsArrayProperty[T]) extends JsArray[T](new BasicDBList){
  override def +=(value:T) = {
    if(!property.isDefinedOnObject(obj.obj))
      property.putUncheckedValue(obj.obj,this.list)
    
    super.+=(value)
  }
}

object JsArray{
  def apply[T](firstValue:T,values:T*) = new JsArray[T].add(firstValue,values:_*)
  
  def apply[T](values:Seq[T]) = new JsArray[T].addAll(values)
  
  def apply[T](values:Array[T]) = new JsArray[T].addAll(values)
  
  //def fromArgs[T](values:T*) = new JsArray[T] ++= values
}
