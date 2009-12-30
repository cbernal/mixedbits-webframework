package net.mixedbits.mongo

import java.util.Date
import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._
import com.mongodb._

trait JsProperty[T]{
  def resolveObject(start:BasicDBObject,create:Boolean):BasicDBObject =
    MongoTools.resolveObject(start,create,propertyPath)
  
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
  
  protected var explicitPropertyName:String = null
  protected def propertyName(newPropertyName:String){
    explicitPropertyName = newPropertyName
  }
  lazy val propertyName:String = 
    if(explicitPropertyName != null)
      explicitPropertyName
    else
      Objects.simpleClassName(this)
    
  lazy val pathParts = propertyName.split('.')
  def propertyPath:Array[String] = pathParts.take(pathParts.length-1).toArray
  def shortName:String = pathParts.last
  
  //operators
  def equals(value:T):MongoConstraint = this == value // this "operator" helps to remove compiler warnings about all of the ==, >=, <= operators
  
  def ==(value:T):MongoConstraint
  def !=(value:T):MongoConstraint
  
  //add support for in, exists, dbref
  
}

class JsArrayProperty[T] extends JsProperty[JsArray[T]]{
  def ==(value:JsArray[T]):MongoConstraint = new MongoPropertyConstraint(propertyName,"",value.list)
  def !=(value:JsArray[T]):MongoConstraint = new MongoPropertyConstraint(propertyName,"$ne",value.list)  
}

class JsStringProperty extends JsProperty[String]{
  def ==(value:String):MongoConstraint = new MongoPropertyConstraint(propertyName,"",value)
  def !=(value:String):MongoConstraint = new MongoPropertyConstraint(propertyName,"$ne",value)
}

abstract class JsNumberProperty[T] extends JsProperty[T]{
  def ==(value:T):MongoConstraint = new MongoPropertyConstraint(propertyName,"",value)
  def !=(value:T):MongoConstraint = new MongoPropertyConstraint(propertyName,"$ne",value)
  def >(value:T):MongoConstraint = new MongoPropertyConstraint(propertyName,"$gt",value)
  def >=(value:T):MongoConstraint = new MongoPropertyConstraint(propertyName,"$gte",value)
  def <(value:T):MongoConstraint = new MongoPropertyConstraint(propertyName,"$lt",value)
  def <=(value:T):MongoConstraint = new MongoPropertyConstraint(propertyName,"$lte",value)
}

class JsIntProperty extends JsNumberProperty[Int]
class JsDoubleProperty extends JsNumberProperty[Double]
class JsDateProperty extends JsNumberProperty[Date]

//simple runtime definition of properties
object JsStringProperty{ def apply(name:String) = new JsStringProperty{propertyName(name)} }
object JsIntProperty{ def apply(name:String) = new JsIntProperty{propertyName(name)} }
object JsDoubleProperty{ def apply(name:String) = new JsDoubleProperty{propertyName(name)} }
object JsDateProperty{ def apply(name:String) = new JsDateProperty{propertyName(name)} }
