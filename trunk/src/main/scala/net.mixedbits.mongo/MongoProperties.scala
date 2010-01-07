package net.mixedbits.mongo

import java.util.Date
import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._
import com.mongodb._

trait JsProperty[T]{
  //data reading / writing
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
  
  //property name / parts
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
  
  //property groups
  def and(property:JsProperty[_]) = 
    new JsPropertyGroup(this,property)
  
  //query operators
  def equals(value:T):MongoConstraint = this == value // this "operator" helps to remove compiler warnings about all of the ==, >=, <= operators
  def ==(value:T):MongoConstraint = new MongoPropertyConstraint(propertyName,"",value)
  def !=(value:T):MongoConstraint = new MongoPropertyConstraint(propertyName,"$ne",value)
  
  //add support for in, exists, dbref
  
  //update operators
  def <~ (value:T):MongoUpdate = 
    new MongoPropertyUpdate(this.propertyName,"$set",value)

}

class JsAnyProperty extends JsProperty[Any]

class JsArrayProperty[T] extends JsProperty[JsArray[T]]{
  override def ==(value:JsArray[T]):MongoConstraint = new MongoPropertyConstraint(propertyName,"",value.list)
  override def !=(value:JsArray[T]):MongoConstraint = new MongoPropertyConstraint(propertyName,"$ne",value.list)
  
  def +=(value:T):MongoUpdate = new MongoPropertyUpdate(propertyName,"$push",value)
  def ++=(value:Seq[T]):MongoUpdate = new MongoPropertyUpdate(propertyName,"$pushAll",if(value.isInstanceOf[JsArray[T]]) value else JsArray(value))
    
  def -=(value:T):MongoUpdate = new MongoPropertyUpdate(propertyName,"$pull",value)
  def --=(value:Seq[T]):MongoUpdate = new MongoPropertyUpdate(propertyName,"$pullAll",if(value.isInstanceOf[JsArray[T]]) value else JsArray(value))
}

class JsStringProperty extends JsProperty[String]

abstract class JsOrderedProperty[T] extends JsProperty[T]{
  def >(value:T):MongoConstraint = new MongoPropertyConstraint(propertyName,"$gt",value)
  def >=(value:T):MongoConstraint = new MongoPropertyConstraint(propertyName,"$gte",value)
  def <(value:T):MongoConstraint = new MongoPropertyConstraint(propertyName,"$lt",value)
  def <=(value:T):MongoConstraint = new MongoPropertyConstraint(propertyName,"$lte",value)  
}

abstract class JsNumberProperty[T <: AnyVal] extends JsOrderedProperty[T]{
  def +=(value:T):MongoUpdate = new MongoPropertyUpdate(propertyName,"$inc",value)
  //def -=(value:T):MongoUpdate = new MongoPropertyUpdate(propertyName,"$inc",-value)
}

class JsIntProperty extends JsNumberProperty[Int]
class JsDoubleProperty extends JsNumberProperty[Double]

class JsDateProperty extends JsOrderedProperty[Date]

//simple runtime definition of properties
object JsAnyProperty{ def apply(name:String) = new JsAnyProperty{propertyName(name)} }
object JsStringProperty{ def apply(name:String) = new JsStringProperty{propertyName(name)} }
object JsIntProperty{ def apply(name:String) = new JsIntProperty{propertyName(name)} }
object JsDoubleProperty{ def apply(name:String) = new JsDoubleProperty{propertyName(name)} }
object JsDateProperty{ def apply(name:String) = new JsDateProperty{propertyName(name)} }
