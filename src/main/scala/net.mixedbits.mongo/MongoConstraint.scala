package net.mixedbits.mongo

import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._
import com.mongodb._

import scala.collection.mutable.ListBuffer


abstract class MongoConstraint{
  def &&(constraint:MongoConstraint) = this and constraint
  def and(constraint:Option[MongoConstraint]):MongoConstraint = 
    constraint.map(new MongoConstraintGroup(this,_)).getOrElse(this)
  def and(constraint:MongoConstraint):MongoConstraint = new MongoConstraintGroup(this,constraint)
  def buildSearchObject:BasicDBObject = applyToSearchObject(new BasicDBObject)
  def applyToSearchObject(obj:BasicDBObject):BasicDBObject
  override def toString = buildSearchObject.toString
}

class MongoConstraintGroup extends MongoConstraint{
  def this(a:MongoConstraint,b:MongoConstraint) = {
    this()
    constraints += a
    constraints += b
  }

  protected val constraints = new ListBuffer[MongoConstraint]
  
  override def and(constraint:MongoConstraint):MongoConstraint =
    this += constraint
  
  def += (constraint:MongoConstraint):MongoConstraint = {
    constraints += constraint
    this
  }
  
  def applyToSearchObject(obj:BasicDBObject):BasicDBObject = {
    for(constraint <- constraints)
      constraint.applyToSearchObject(obj)
    obj
  }
  
}

class MongoPropertyConstraint(key:String,operation:String,value:Any) extends MongoConstraint{
  def applyToSearchObject(obj:BasicDBObject):BasicDBObject = {
    if(operation == null || operation == "")
      obj.put(key,value)
    else{
      if(obj.containsKey(key))
        obj.get(key).asInstanceOf[BasicDBObject].put(operation,value)
      else
        obj.put(key,new BasicDBObject(operation,value))
    }
    obj
  }
}
