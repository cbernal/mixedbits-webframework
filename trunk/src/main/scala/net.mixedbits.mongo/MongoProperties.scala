package net.mixedbits.mongo

import java.util.Date
import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._
import com.mongodb._

trait JsProperty[T]{
  def resolveObject(start:BasicDBObject,create:Boolean):BasicDBObject =
    MongoTools.resolveObject(start,create,fieldPath)
  
  def putUncheckedValue[X](start:BasicDBObject,value:X):X = {
    resolveObject(start,true).put(shortName,value)
    value
  }
  
  def readUncheckedValue[X](start:BasicDBObject):Option[X] = {
    val currentObject = resolveObject(start,false)
    if(currentObject == null || !currentObject.containsField(shortName))
      return None
    else
      return Some(currentObject.get(shortName).asInstanceOf[X])
  }
  
  def updateValue(start:BasicDBObject,value:T):T = 
    putUncheckedValue(start,value)
  
  def readValue(start:BasicDBObject):Option[T] = 
    readUncheckedValue(start)
  
  protected var explicitFieldName:String = null
  protected def fieldName(newFieldName:String){
    explicitFieldName = newFieldName
  }
  def fieldName:String = 
    if(explicitFieldName == null)
      Objects.simpleClassName(this)
    else
      explicitFieldName
    
  lazy val pathParts = fieldName.split('.')
  def fieldPath:Array[String] = pathParts.take(pathParts.length-1).toArray
  def shortName:String = pathParts.last
  
  //operators
  def equals(value:T):MongoConstraint = this == value // this "operator" helps to remove compiler warnings about all of the ==, >=, <= operators
  
  def ==(value:T):MongoConstraint
  def !=(value:T):MongoConstraint
  
  //add support for in, exists, dbref
  
}

class JsArrayProperty[T] extends JsProperty[JsArray[T]]{
  def ==(value:JsArray[T]):MongoConstraint = new MongoPropertyConstraint(fieldName,"",value.list)
  def !=(value:JsArray[T]):MongoConstraint = new MongoPropertyConstraint(fieldName,"$ne",value.list)  
}

class JsStringProperty extends JsProperty[String]{
  def ==(value:String):MongoConstraint = new MongoPropertyConstraint(fieldName,"",value)
  def !=(value:String):MongoConstraint = new MongoPropertyConstraint(fieldName,"$ne",value)
}

abstract class JsNumberProperty[T] extends JsProperty[T]{
  def ==(value:T):MongoConstraint = new MongoPropertyConstraint(fieldName,"",value)
  def !=(value:T):MongoConstraint = new MongoPropertyConstraint(fieldName,"$ne",value)
  def >(value:T):MongoConstraint = new MongoPropertyConstraint(fieldName,"$gt",value)
  def >=(value:T):MongoConstraint = new MongoPropertyConstraint(fieldName,"$gte",value)
  def <(value:T):MongoConstraint = new MongoPropertyConstraint(fieldName,"$lt",value)
  def <=(value:T):MongoConstraint = new MongoPropertyConstraint(fieldName,"$lte",value)
}

class JsIntProperty extends JsNumberProperty[Int]
class JsDoubleProperty extends JsNumberProperty[Double]
class JsDateProperty extends JsNumberProperty[Date]

//simple runtime definition of properties
object JsStringProperty{ def apply(name:String) = new JsStringProperty{fieldName(name)} }
object JsIntProperty{ def apply(name:String) = new JsIntProperty{fieldName(name)} }
object JsDoubleProperty{ def apply(name:String) = new JsDoubleProperty{fieldName(name)} }
object JsDateProperty{ def apply(name:String) = new JsDateProperty{fieldName(name)} }
