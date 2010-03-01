package net.mixedbits.json

import java.util.Date

import net.mixedbits.tools._
import net.mixedbits.tools.Numbers._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._

import com.mongodb._

sealed trait JsProperty[T]{
  //data reading / writing
  def resolveObject(start:DBObject,create:Boolean):DBObject =
    JsTools.resolveObject(start,create,propertyPath)
  
  def putUncheckedValue[X](start:DBObject,value:X):X = {
    resolveObject(start,true).put(shortName,JsTools.rawValue(value))
    value
  }
  
  def readUncheckedValue[X](start:DBObject):Option[X] = {
    val currentObject = resolveObject(start,false)
    if(currentObject == null || !currentObject.containsField(shortName))
      None
    else
      toOption(currentObject.get(shortName).asInstanceOf[X])
  }
  
  def isDefinedOnObject(obj:DBObject):Boolean = {
    val currentObject = resolveObject(obj,false)
    if(currentObject == null || !currentObject.containsField(shortName))
      false
    else
      true
  }
  
  def removeValue(obj:DBObject){
    toOption(resolveObject(obj,false)).map(_.removeField(shortName))
  }
  
  def updateValue(start:DBObject,value:T):T = 
    putUncheckedValue(start,value)
  
  def readValue(start:DBObject):Option[T] = 
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
  
  def fromString(value:String):Option[T] = None
  
  //property groups
  def and(property:JsProperty[_]) = 
    new JsPropertyGroup(this,property)
  def &&(property:JsProperty[_]) = 
    new JsPropertyGroup(this,property)
  
  //query operators
  def equals(value:T):JsConstraint = this == value // this "operator" helps to remove compiler warnings about all of the ==, >=, <= operators
  def ==(value:T):JsConstraint = new JsPropertyConstraint(propertyName,"",value)
  def !=(value:T):JsConstraint = new JsPropertyConstraint(propertyName,"$ne",value)
  
  def in (values:Seq[T]):JsConstraint = new JsPropertyConstraint(propertyName,"$in",JsArray(values).list)
  def notIn (values:Seq[T]):JsConstraint = new JsPropertyConstraint(propertyName,"$nin",JsArray(values).list)
  
  //add support exists, dbref
  def exists(value:Boolean):JsConstraint = new JsPropertyConstraint(propertyName,"$exists",value)
  
  
  //update operators
  def <~ (value:T):JsUpdate = 
    new JsPropertyUpdate(this.propertyName,"$set",value,(obj=> obj(this) = value))
  
  def <~ (value:Option[T]):JsUpdate = value match {
    case Some(v) => new JsPropertyUpdate(this.propertyName,"$set",v,(obj=> obj(this) = v))
    case None => new JsPropertyUpdate(this.propertyName,"$set",null,(obj=> obj(this) = None))//new JsPropertyUpdate(this.propertyName,"$unset",1) //not supported yet...
  }
  
  override def toString() = "JsProperty("+propertyName+")"

}

/*
class JsIdProperty extends JsProperty[String]{
  def this(name:String) = {this();propertyName(name)}
  def this(parent:JsProperty[_]) = {this();propertyName(parent.propertyName+"."+Objects.objectPath(this).last)}
  def this(parent:JsProperty[_],name:String) = {this();propertyName(parent.propertyName+"."+name)}
  override def ==(value:String):JsConstraint = new JsPropertyConstraint(propertyName,"",MongoTools.marshalId(value))
  override def !=(value:String):JsConstraint = new JsPropertyConstraint(propertyName,"$ne",MongoTools.marshalId(value))
}
*/
class JsAnyProperty extends JsProperty[AnyRef]{
  def this(name:String) = {this();propertyName(name)}
  def this(parent:JsProperty[_]) = {this();propertyName(parent.propertyName+"."+Objects.objectPath(this).last)}
  def this(parent:JsProperty[_],name:String) = {this();propertyName(parent.propertyName+"."+name)}
}

class JsObjectProperty extends JsProperty[JsObject]{
  def this(name:String) = {this();propertyName(name)}
  def this(parent:JsProperty[_]) = {this();propertyName(parent.propertyName+"."+Objects.objectPath(this).last)}
  def this(parent:JsProperty[_],name:String) = {this();propertyName(parent.propertyName+"."+name)}
  
  override def fromString(value:String):Option[JsObject] = attempt{JsObject.parse(value)}
}

class JsArrayProperty[T] extends JsProperty[JsArray[T]]{
  def this(name:String) = {this();propertyName(name)}
  def this(parent:JsProperty[_]) = {this();propertyName(parent.propertyName+"."+Objects.objectPath(this).last)}
  def this(parent:JsProperty[_],name:String) = {this();propertyName(parent.propertyName+"."+name)}
  override def ==(value:JsArray[T]):JsConstraint = new JsPropertyConstraint(propertyName,"",value.list)
  override def !=(value:JsArray[T]):JsConstraint = new JsPropertyConstraint(propertyName,"$ne",value.list)
  
  def contains(value:T):JsConstraint = new JsPropertyConstraint(propertyName,"",value)
  def containsAll(values:Seq[T]):JsConstraint = new JsPropertyConstraint(propertyName,"$all",JsArray(values).list)
  
  def +(value:T):JsUpdate = this += value
  def ++(value:Seq[T]):JsUpdate = this ++= value
  
  def -(value:T):JsUpdate = this -= value
  def --(value:Seq[T]):JsUpdate = this ++= value
  
  def +=(value:T):JsUpdate =
    new JsPropertyUpdate(
      propertyName,"$push",value,
      (obj=> obj(this) += value)
      )
  def ++=(value:Seq[T]):JsUpdate =
    new JsPropertyUpdate(
      propertyName,"$pushAll",if(value.isInstanceOf[JsArray[_]]) value else JsArray(value),
      (obj=> obj(this) ++= value)
      )
    
  def -=(value:T):JsUpdate =
    new JsPropertyUpdate(
      propertyName,"$pull",value,
      (obj=> obj(this) -= value)
      )
  def --=(value:Seq[T]):JsUpdate =
    new JsPropertyUpdate(
      propertyName,"$pullAll",if(value.isInstanceOf[JsArray[_]]) value else JsArray(value),
      (obj=> obj(this) --= value)
      )
}

class JsStringProperty extends JsProperty[String]{
  def this(name:String) = {this();propertyName(name)}
  def this(parent:JsProperty[_]) = {this();propertyName(parent.propertyName+"."+Objects.objectPath(this).last)}
  def this(parent:JsProperty[_],name:String) = {this();propertyName(parent.propertyName+"."+name)}
  
  override def fromString(value:String):Option[String] = Some(value)
}

class JsBooleanProperty extends JsProperty[Boolean]{
  def this(name:String) = {this();propertyName(name)}
  def this(parent:JsProperty[_]) = {this();propertyName(parent.propertyName+"."+Objects.objectPath(this).last)}
  def this(parent:JsProperty[_],name:String) = {this();propertyName(parent.propertyName+"."+name)}
  
  override def fromString(value:String):Option[Boolean] = value.parseBoolean
}

abstract class JsOrderedProperty[T] extends JsProperty[T]{
  def >(value:T):JsConstraint = new JsPropertyConstraint(propertyName,"$gt",value)
  def >=(value:T):JsConstraint = new JsPropertyConstraint(propertyName,"$gte",value)
  def <(value:T):JsConstraint = new JsPropertyConstraint(propertyName,"$lt",value)
  def <=(value:T):JsConstraint = new JsPropertyConstraint(propertyName,"$lte",value)
}

abstract class JsNumberProperty[T <: AnyVal] extends JsOrderedProperty[T]{
  def +(value:T):JsUpdate = this += value
  def -(value:T):JsUpdate = this -= value
  def +=(value:T):JsUpdate
  def -=(value:T):JsUpdate
}

class JsIntProperty extends JsNumberProperty[Int]{
  def this(name:String) = {this();propertyName(name)}
  def this(parent:JsProperty[_]) = {this();propertyName(parent.propertyName+"."+Objects.objectPath(this).last)}
  def this(parent:JsProperty[_],name:String) = {this();propertyName(parent.propertyName+"."+name)}
  def +=(value:Int):JsUpdate =
    new JsPropertyUpdate(propertyName,"$inc",value,(obj => for(orig <- obj(this)) obj(this) = orig+value))
  def -=(value:Int):JsUpdate = 
    new JsPropertyUpdate(propertyName,"$inc",-value,(obj => for(orig <- obj(this)) obj(this) = orig-value))
    
  override def fromString(value:String):Option[Int] = value.parseInt
}

class JsLongProperty extends JsNumberProperty[Long]{
  def this(name:String) = {this();propertyName(name)}
  def this(parent:JsProperty[_]) = {this();propertyName(parent.propertyName+"."+Objects.objectPath(this).last)}
  def this(parent:JsProperty[_],name:String) = {this();propertyName(parent.propertyName+"."+name)}
  def +=(value:Long):JsUpdate =
    new JsPropertyUpdate(propertyName,"$inc",value,(obj => for(orig <- obj(this)) obj(this) = orig+value))
  def -=(value:Long):JsUpdate = 
    new JsPropertyUpdate(propertyName,"$inc",-value,(obj => for(orig <- obj(this)) obj(this) = orig-value))
    
  override def fromString(value:String):Option[Long] = value.parseLong

  override def readValue(start:DBObject):Option[Long] = 
    readUncheckedValue[Any](start) match {
      case Some(value:Int) => Some(value.toLong)
      case Some(value:Long) => Some(value)
      case _ => None
    }
}

class JsDoubleProperty extends JsNumberProperty[Double]{
  def this(name:String) = {this();propertyName(name)}
  def +=(value:Double):JsUpdate =
    new JsPropertyUpdate(propertyName,"$inc",value,(obj => for(orig <- obj(this)) obj(this) = orig+value))
  def -=(value:Double):JsUpdate = 
    new JsPropertyUpdate(propertyName,"$inc",-value,(obj => for(orig <- obj(this)) obj(this) = orig-value))
    
  override def fromString(value:String):Option[Double] = value.parseDouble
}

class JsDateProperty extends JsOrderedProperty[Date]{
  def this(name:String) = {this();propertyName(name)}
  def this(parent:JsProperty[_]) = {this();propertyName(parent.propertyName+"."+Objects.objectPath(this).last)}
  def this(parent:JsProperty[_],name:String) = {this();propertyName(parent.propertyName+"."+name)}
}

//simple runtime definition of properties
object JsAnyProperty{ def apply(name:String) = new JsAnyProperty(name) }
object JsObjectProperty{ def apply(name:String) = new JsObjectProperty(name) }
object JsStringProperty{ def apply(name:String) = new JsStringProperty(name) }
object JsBooleanProperty{ def apply(name:String) = new JsBooleanProperty(name) }
object JsIntProperty{ def apply(name:String) = new JsIntProperty(name) }
object JsLongProperty{ def apply(name:String) = new JsLongProperty(name) }
object JsDoubleProperty{ def apply(name:String) = new JsDoubleProperty(name) }
object JsDateProperty{ def apply(name:String) = new JsDateProperty(name) }


class JsUrlProperty extends JsStringProperty{
  
  override def fromString(value:String):Option[String] =
    if(value startsWith "http://")
      Some(value)
    else if(value startsWith "https://")
      Some(value)
    else
      None

}


class JsEmailProperty extends JsStringProperty{

  override def fromString(value:String):Option[String] = 
    if(value.contains("@") && value.lastIndexOf(".") > value.lastIndexOf("@"))
      Some(value)
    else
      None

}
