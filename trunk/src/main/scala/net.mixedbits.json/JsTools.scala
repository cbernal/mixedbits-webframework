package net.mixedbits.json

import com.mongodb._
import org.bson.types._

object JsTools{
  
  def generateId():String =
    new ObjectId().toString()
  
  def marshalId(value:String):AnyRef = { 
    if(value == null || value.length != 24)
      return value
    
    try{
      new ObjectId(value)
    } catch {
      case _ => value
    }
  }
  
  
  
  def resolveObject(start:DBObject,create:Boolean,fieldPath:Array[String]):DBObject = {
    var currentObject = start
    for(field <- fieldPath){
      if(currentObject == null){
        return null
      }
      else if(currentObject.containsField(field)){
        currentObject = currentObject.get(field).asInstanceOf[DBObject]
      }
      else if(create){
        val newObject = new BasicDBObject
        currentObject.put(field,newObject)
        currentObject = newObject
      }
      else{
        return null
      }
    }
    currentObject
  }
  
  def rawValue(value:Any):Any = value match {
    case null => null
    case option:Option[_] => option.map(rawValue) getOrElse null
    case obj:JsObject => obj.obj
    case array:JsArray[_] => array.list
    case _ => value
  }
    
  def wrappedValue(value:Any):Any = 
    if(value.isInstanceOf[BasicDBList])
      new JsArray(value.asInstanceOf[BasicDBList])
    else if(value.isInstanceOf[DBObject])
      new JsObject(value.asInstanceOf[DBObject])
    else
      value
  
}
