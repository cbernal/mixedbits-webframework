package net.mixedbits.json

import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._

import com.mongodb._

import scala.collection.mutable.ListBuffer

abstract class JsUpdate{
  def &&(update:JsUpdate) = this and update
  def and(update:Option[JsUpdate]):JsUpdateGroup = 
    update.map(new JsUpdateGroup(this,_)).getOrElse(new JsUpdateGroup(this))
  def and(update:JsUpdate):JsUpdateGroup = new JsUpdateGroup(this,update)
  def applyToObject(obj:JsObject):JsObject
  def buildUpdateObject:BasicDBObject = applyToUpdateObject(new BasicDBObject)
  def applyToUpdateObject(obj:BasicDBObject):BasicDBObject
  override def toString = buildUpdateObject.toString
}

class JsUpdateGroup extends JsUpdate{
  def this(a:JsUpdate) = {
    this()
    updates += a
  }
  def this(a:JsUpdate,b:JsUpdate) = {
    this()
    updates += a
    updates += b
  }
  
  protected val updates = new ListBuffer[JsUpdate]
  
  override def and(update:JsUpdate):JsUpdateGroup = {
    updates += update
    this
  }
  
  def applyToObject(obj:JsObject) = {
    for(update <- updates)
      update.applyToObject(obj)
    obj
  }
  
  def applyToUpdateObject(obj:BasicDBObject):BasicDBObject = {
    for(update <- updates)
      update.applyToUpdateObject(obj)
    obj
  }
}

class JsPropertyUpdate(key:String,operation:String,value:Any,applicator:JsObject=>Any) extends JsUpdate{
  def applyToObject(obj:JsObject) = {applicator(obj);obj}
  def applyToUpdateObject(obj:BasicDBObject):BasicDBObject = {
    
    if(obj.containsKey(operation))
      obj.get(operation).asInstanceOf[BasicDBObject].put(key,value)
    else
      obj.put(operation,new BasicDBObject(key,value))

    obj
  }
}
