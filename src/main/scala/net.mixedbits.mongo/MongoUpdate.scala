package net.mixedbits.mongo

import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._
import com.mongodb._

import scala.collection.mutable.ListBuffer

abstract class MongoUpdate{
  def and(update:MongoUpdate):MongoUpdateGroup = new MongoUpdateGroup(this,update)
  def buildUpdateObject:BasicDBObject = applyToUpdateObject(new BasicDBObject)
  def applyToUpdateObject(obj:BasicDBObject):BasicDBObject
  override def toString = buildUpdateObject.toString
}
      
class MongoUpdateGroup extends MongoUpdate{
  def this(a:MongoUpdate,b:MongoUpdate) = {
    this()
    updates += a
    updates += b
  }
  
  protected val updates = new ListBuffer[MongoUpdate]
  
  override def and(update:MongoUpdate):MongoUpdateGroup =
    this += update
  
  def += (update:MongoUpdate):MongoUpdateGroup = {
    updates += update
    this
  }
  
  def applyToUpdateObject(obj:BasicDBObject):BasicDBObject = {
    for(update <- updates)
      update.applyToUpdateObject(obj)
    obj
  }
}

class MongoPropertyUpdate(key:String,operation:String,value:Any) extends MongoUpdate{
  def applyToUpdateObject(obj:BasicDBObject):BasicDBObject = {
    
    if(obj.containsKey(operation))
      obj.get(operation).asInstanceOf[BasicDBObject].put(key,value)
    else
      obj.put(operation,new BasicDBObject(key,value))

    obj
  }
}
