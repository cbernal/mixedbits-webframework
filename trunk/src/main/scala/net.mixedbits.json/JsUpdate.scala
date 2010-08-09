package net.mixedbits.json

import net.mixedbits.tools._

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
  def this(updates:Seq[JsUpdate]) = {
    this()
    _updates ++= updates
  }
  
  def this(a:JsUpdate) = {
    this()
    _updates += a
  }
  def this(a:JsUpdate,b:JsUpdate) = {
    this()
    _updates += a
    _updates += b
  }
  
  protected val _updates = new ListBuffer[JsUpdate]
  
  override def and(update:JsUpdate):JsUpdateGroup = {
    _updates += update
    this
  }
  
  def applyToObject(obj:JsObject) = {
    for(update <- _updates)
      update.applyToObject(obj)
    obj
  }
  
  def applyToUpdateObject(obj:BasicDBObject):BasicDBObject = {
    for(update <- _updates)
      update.applyToUpdateObject(obj)
    obj
  }
}

class JsPropertyUpdate(key:String,operation:String,value:Any,applicator:JsObject=>Any) extends JsUpdate{
  def applyToObject(obj:JsObject) = {applicator(obj);obj}
  def applyToUpdateObject(obj:BasicDBObject):BasicDBObject = {

    obj.get(operation) match {
      case null => obj.put(operation,new BasicDBObject(key,value))
      case existingObject:BasicDBObject => existingObject.put(key,value)
    }

    obj
  }
}
