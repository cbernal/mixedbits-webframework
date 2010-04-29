package net.mixedbits.json

import com.mongodb._


object JsTools{
  
  def generateId():String =
    new com.mongodb.ObjectId().toString()
  
  def marshalId(value:String):AnyRef = { 
    if(value == null || value.length != 24)
      return value
    
    //this method will convert strings that are longer than an object id if the first 12 hex encoded bytes are valid
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
    if(value.isInstanceOf[Option[_]])
      value.asInstanceOf[Option[_]].map(rawValue) getOrElse null
    else if(value.isInstanceOf[JsObject])
      value.asInstanceOf[JsObject].obj
    else if(value.isInstanceOf[JsArray[_]])
      value.asInstanceOf[JsArray[_]].list
    else
      value
    
  def wrappedValue(value:Any):Any = 
    if(value.isInstanceOf[BasicDBList])
      new JsArray(value.asInstanceOf[BasicDBList])
    else if(value.isInstanceOf[DBObject])
      new JsObject(value.asInstanceOf[DBObject])
    else
      value
  
}
