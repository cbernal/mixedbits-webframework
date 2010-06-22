package net.mixedbits.json

import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._

import com.mongodb._

import scala.collection.mutable.ListBuffer

abstract class JsConstraint{
  def &&(constraint:JsConstraint) = this and constraint
  def and(constraint:Option[JsConstraint]):JsConstraint = 
    constraint.map(new JsConstraintGroup(this,_)).getOrElse(this)
  def and(constraint:JsConstraint):JsConstraint = new JsConstraintGroup(this,constraint)
  def buildSearchObject:BasicDBObject = applyToSearchObject(new BasicDBObject)
  def applyToSearchObject(obj:BasicDBObject):BasicDBObject
  override def toString = buildSearchObject.toString
}

class JsConstraintGroup extends JsConstraint{
  def this(a:JsConstraint,b:JsConstraint) = {
    this()
    constraints += a
    constraints += b
  }

  protected val constraints = new ListBuffer[JsConstraint]
  
  override def and(constraint:JsConstraint):JsConstraint =
    this += constraint
  
  def += (constraint:JsConstraint):JsConstraint = {
    constraints += constraint
    this
  }
  
  def applyToSearchObject(obj:BasicDBObject):BasicDBObject = {
    for(constraint <- constraints)
      constraint.applyToSearchObject(obj)
    obj
  }
  
}

class JsPropertyConstraint(key:String,operation:String,value:Any) extends JsConstraint{
  def applyToSearchObject(obj:BasicDBObject):BasicDBObject = {
    if(operation == null || operation == "")
      obj.put(key,value)
    else{
      obj.get(key) match {
        case null => obj.put(key,new BasicDBObject(operation,value))
        case existingObject:BasicDBObject => existingObject.put(operation,value)
      }
    }
    obj
  }
}
