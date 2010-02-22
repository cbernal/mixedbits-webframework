package net.mixedbits.json

import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._

import com.mongodb._
import com.mongodb.util._


class JsObject(baseObject:DBObject){
  def this() = this(new BasicDBObject)
  def add(firstValue:(String,Any),values:(String,Any)*) = {
    update(firstValue._1,firstValue._2)
    for( (key,value) <- values)
      update(key,value)
    this
  }
  
  val obj:DBObject = baseObject
  
  def apply(updates:JsUpdate):this.type = {
    updates.applyToObject(this)
    this
  }
  
  def apply[T](property:JsProperty[T]):Option[T] =
    property.readValue(obj)
  def apply[T](property:JsProperty[T],defaultValue:T):T =
    property.readValue(obj).getOrElse(defaultValue)
  
  def update[T](property:JsProperty[T],value:T):this.type = {
    property.updateValue(obj,value)
    this
  }
  def update[T](property:JsProperty[T],value:Option[T]):this.type = {
    value match {
      case Some(v) => update(property,v)
      case None => property.removeValue(obj)
    }
    this
  }
  def update[T](propertyName:String,value:T):this.type = {
    obj.put(propertyName,JsTools.rawValue(value))
    this
  }

  def apply[T](property:JsArrayProperty[T]):JsArray[T] = {
    property.readUncheckedValue(obj) match {
      case Some(list) => new JsArray[T](list)
      case None => new JsPhantomArray(this,property)
    }
  }
  def update[T](property:JsArrayProperty[T],value:JsArray[T]):this.type = {
    property.putUncheckedValue(obj,value.list)
    this
  }
  
  def contains(property:JsProperty[_]):Boolean = 
    property.isDefinedOnObject(obj)
  
  def toJson():String = obj.toString
  override def toString = toJson
}

object JsObject{
  def apply(updates:JsUpdate):JsObject = 
    updates.applyToObject(new JsObject)
  
  def apply() = new JsObject
  def apply(firstValue:(String,Any),values:(String,Any)*) = new JsObject().add(firstValue,values:_*)
  
  def parse(data:String) = new JsObject(JSON.parse(data).asInstanceOf[BasicDBObject])
}
