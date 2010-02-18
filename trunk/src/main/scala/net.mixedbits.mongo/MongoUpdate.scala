package net.mixedbits.mongo

import net.mixedbits.json._
import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._
import com.mongodb._

import scala.collection.mutable.ListBuffer

abstract class MongoUpdate{
  def &&(update:MongoUpdate) = this and update
  def and(update:Option[MongoUpdate]):MongoUpdateGroup = 
    update.map(new MongoUpdateGroup(this,_)).getOrElse(new MongoUpdateGroup(this))
  def and(update:MongoUpdate):MongoUpdateGroup = new MongoUpdateGroup(this,update)
  def applyToObject(obj:JsObject):JsObject
  def buildUpdateObject:BasicDBObject = applyToUpdateObject(new BasicDBObject)
  def applyToUpdateObject(obj:BasicDBObject):BasicDBObject
  override def toString = buildUpdateObject.toString
}
      
class MongoUpdateGroup extends MongoUpdate{
  def this(a:MongoUpdate) = {
    this()
    updates += a
  }
  def this(a:MongoUpdate,b:MongoUpdate) = {
    this()
    updates += a
    updates += b
  }
  
  protected val updates = new ListBuffer[MongoUpdate]
  
  override def and(update:MongoUpdate):MongoUpdateGroup = {
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

class MongoPropertyUpdate(key:String,operation:String,value:Any,applicator:JsObject=>Any) extends MongoUpdate{
  def applyToObject(obj:JsObject) = {applicator(obj);obj}
  def applyToUpdateObject(obj:BasicDBObject):BasicDBObject = {
    
    if(obj.containsKey(operation))
      obj.get(operation).asInstanceOf[BasicDBObject].put(key,value)
    else
      obj.put(operation,new BasicDBObject(key,value))

    obj
  }
}
