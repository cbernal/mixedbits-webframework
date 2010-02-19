package net.mixedbits.json

import com.mongodb._


object JsTools{
  
  def generateId():String =
    new com.mongodb.ObjectId().toString()
  
  def marshalId(value:String):AnyRef = {
    val objectId = ObjectId.massageToObjectId(value)
    if(objectId!=null)
      objectId
    else
      value
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
  
  def rawValue(value:Any):Any = 
    if(value.isInstanceOf[JsObject])
      value.asInstanceOf[JsObject].obj
    else if(value.isInstanceOf[JsArray[_]])
      value.asInstanceOf[JsArray[_]].list
    else
      value
  
}
