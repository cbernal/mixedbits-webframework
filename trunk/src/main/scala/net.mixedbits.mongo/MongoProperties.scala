package net.mixedbits.mongo

import java.util.Date
import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._
import com.mongodb._

trait JsProperty[T]{
  //data reading / writing
  def resolveObject(start:DBObject,create:Boolean):DBObject =
    MongoTools.resolveObject(start,create,propertyPath)
  
  def putUncheckedValue[X](start:DBObject,value:X):X = {
    resolveObject(start,true).put(shortName,MongoTools.rawValue(value))
    value
  }
  
  def readUncheckedValue[X](start:DBObject):Option[X] = {
    val currentObject = resolveObject(start,false)
    if(currentObject == null || !currentObject.containsField(shortName))
      None
    else
      toOption(currentObject.get(shortName).asInstanceOf[X])
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
  
  //property groups
  def and(property:JsProperty[_]) = 
    new JsPropertyGroup(this,property)
  def &&(property:JsProperty[_]) = 
    new JsPropertyGroup(this,property)
  
  //query operators
  def equals(value:T):MongoConstraint = this == value // this "operator" helps to remove compiler warnings about all of the ==, >=, <= operators
  def ==(value:T):MongoConstraint = new MongoPropertyConstraint(propertyName,"",value)
  def !=(value:T):MongoConstraint = new MongoPropertyConstraint(propertyName,"$ne",value)
  
  //add support for in, exists, dbref
  
  //update operators
  def <~ (value:T):MongoUpdate = 
    new MongoPropertyUpdate(this.propertyName,"$set",value,(obj=> obj(this) = value))
  
  def <~ (value:Option[T]):MongoUpdate = value match {
    case Some(v) => new MongoPropertyUpdate(this.propertyName,"$set",v,(obj=> obj(this) = v))
    case None => new MongoPropertyUpdate(this.propertyName,"$set",null,(obj=> error("not supported!")))//new MongoPropertyUpdate(this.propertyName,"$unset",1) //not supported yet...
  }
  
  override def toString() = "JsProperty("+propertyName+")"

}

class JsSelectableProperty extends JsProperty[Nothing]{
  def this(name:String) = {this();propertyName(name)}
  def this(parent:JsProperty[_]) = {this();propertyName(parent.propertyName+"."+Objects.objectPath(this).last)}
  def this(parent:JsProperty[_],name:String) = {this();propertyName(parent.propertyName+"."+name)}
}

class JsAnyProperty extends JsProperty[Any]{
  def this(name:String) = {this();propertyName(name)}
  def this(parent:JsProperty[_]) = {this();propertyName(parent.propertyName+"."+Objects.objectPath(this).last)}
  def this(parent:JsProperty[_],name:String) = {this();propertyName(parent.propertyName+"."+name)}
}

class JsObjectProperty extends JsProperty[JsObject]{
  def this(name:String) = {this();propertyName(name)}
  def this(parent:JsProperty[_]) = {this();propertyName(parent.propertyName+"."+Objects.objectPath(this).last)}
  def this(parent:JsProperty[_],name:String) = {this();propertyName(parent.propertyName+"."+name)}
}

class JsArrayProperty[T] extends JsProperty[JsArray[T]]{
  def this(name:String) = {this();propertyName(name)}
  def this(parent:JsProperty[_]) = {this();propertyName(parent.propertyName+"."+Objects.objectPath(this).last)}
  def this(parent:JsProperty[_],name:String) = {this();propertyName(parent.propertyName+"."+name)}
  override def ==(value:JsArray[T]):MongoConstraint = new MongoPropertyConstraint(propertyName,"",value.list)
  override def !=(value:JsArray[T]):MongoConstraint = new MongoPropertyConstraint(propertyName,"$ne",value.list)
  
  def +(value:T):MongoUpdate = this += value
  def ++(value:Seq[T]):MongoUpdate = this ++= value
  
  def -(value:T):MongoUpdate = this -= value
  def --(value:Seq[T]):MongoUpdate = this ++= value
  
  def +=(value:T):MongoUpdate =
    new MongoPropertyUpdate(
      propertyName,"$push",value,
      (obj=> obj(this) += value)
      )
  def ++=(value:Seq[T]):MongoUpdate =
    new MongoPropertyUpdate(
      propertyName,"$pushAll",if(value.isInstanceOf[JsArray[_]]) value else JsArray(value),
      (obj=> obj(this) ++= value)
      )
    
  def -=(value:T):MongoUpdate =
    new MongoPropertyUpdate(
      propertyName,"$pull",value,
      (obj=> obj(this) -= value)
      )
  def --=(value:Seq[T]):MongoUpdate =
    new MongoPropertyUpdate(
      propertyName,"$pullAll",if(value.isInstanceOf[JsArray[_]]) value else JsArray(value),
      (obj=> error("not supported!"))
      )
}

class JsStringProperty extends JsProperty[String]{
  def this(name:String) = {this();propertyName(name)}
  def this(parent:JsProperty[_]) = {this();propertyName(parent.propertyName+"."+Objects.objectPath(this).last)}
  def this(parent:JsProperty[_],name:String) = {this();propertyName(parent.propertyName+"."+name)}
}

class JsBooleanProperty extends JsProperty[Boolean]{
  def this(name:String) = {this();propertyName(name)}
  def this(parent:JsProperty[_]) = {this();propertyName(parent.propertyName+"."+Objects.objectPath(this).last)}
  def this(parent:JsProperty[_],name:String) = {this();propertyName(parent.propertyName+"."+name)}
}

abstract class JsOrderedProperty[T] extends JsProperty[T]{
  def >(value:T):MongoConstraint = new MongoPropertyConstraint(propertyName,"$gt",value)
  def >=(value:T):MongoConstraint = new MongoPropertyConstraint(propertyName,"$gte",value)
  def <(value:T):MongoConstraint = new MongoPropertyConstraint(propertyName,"$lt",value)
  def <=(value:T):MongoConstraint = new MongoPropertyConstraint(propertyName,"$lte",value)
}

abstract class JsNumberProperty[T <: AnyVal] extends JsOrderedProperty[T]{
  def +(value:T):MongoUpdate = this += value
  def -(value:T):MongoUpdate = this -= value
  def +=(value:T):MongoUpdate
  def -=(value:T):MongoUpdate
}

class JsIntProperty extends JsNumberProperty[Int]{
  def this(name:String) = {this();propertyName(name)}
  def this(parent:JsProperty[_]) = {this();propertyName(parent.propertyName+"."+Objects.objectPath(this).last)}
  def this(parent:JsProperty[_],name:String) = {this();propertyName(parent.propertyName+"."+name)}
  def +=(value:Int):MongoUpdate =
    new MongoPropertyUpdate(propertyName,"$inc",value,(obj => for(orig <- obj(this)) obj(this) = orig+value))
  def -=(value:Int):MongoUpdate = 
    new MongoPropertyUpdate(propertyName,"$inc",-value,(obj => for(orig <- obj(this)) obj(this) = orig-value))
}
class JsDoubleProperty extends JsNumberProperty[Double]{
  def this(name:String) = {this();propertyName(name)}
  def +=(value:Double):MongoUpdate =
    new MongoPropertyUpdate(propertyName,"$inc",value,(obj => for(orig <- obj(this)) obj(this) = orig+value))
  def -=(value:Double):MongoUpdate = 
    new MongoPropertyUpdate(propertyName,"$inc",-value,(obj => for(orig <- obj(this)) obj(this) = orig-value))
}

class JsDateProperty extends JsOrderedProperty[Date]{
  def this(name:String) = {this();propertyName(name)}
  def this(parent:JsProperty[_]) = {this();propertyName(parent.propertyName+"."+Objects.objectPath(this).last)}
  def this(parent:JsProperty[_],name:String) = {this();propertyName(parent.propertyName+"."+name)}
}

//simple runtime definition of properties
object JsAnyProperty{ def apply(name:String) = new JsAnyProperty(name) }
object JsSelectableProperty{ def apply(name:String) = new JsSelectableProperty(name) }
object JsObjectProperty{ def apply(name:String) = new JsObjectProperty(name) }
object JsStringProperty{ def apply(name:String) = new JsStringProperty(name) }
object JsBooleanProperty{ def apply(name:String) = new JsBooleanProperty(name) }
object JsIntProperty{ def apply(name:String) = new JsIntProperty(name) }
object JsDoubleProperty{ def apply(name:String) = new JsDoubleProperty(name) }
object JsDateProperty{ def apply(name:String) = new JsDateProperty(name) }
