package net.mixedbits.mongo

import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._
import com.mongodb._

import scala.collection.mutable.ListBuffer

abstract class MongoDatabase(name:String){
  def this() = this(null)
  
  def databaseName:String = 
    if(name == null)
      Objects.simpleClassName(this)
    else
      name

  def getDatabase = connection.getDB(databaseName)
  def withDatabase[T](f: DB=>T):T = f(getDatabase)
  
  protected def createConnection:Mongo
  lazy val connection = createConnection
  
  protected def server(host:String) = 
    new Mongo(new DBAddress(host,"test"))
  
  protected def server(host:String,port:Int) = 
    new Mongo(new DBAddress(host,port,"test"))
  
  protected def serverPair(leftHost:String,rightHost:String) = 
    new Mongo(new DBAddress(leftHost,"test"),new DBAddress(rightHost,"test"))
  
  protected def serverPair(leftHost:(String,Int),rightHost:(String,Int)) = 
    new Mongo(new DBAddress(leftHost._1,leftHost._2,"test"),new DBAddress(rightHost._1,rightHost._2,"test"))
}


object Mongo{
  object Error{
    object Message extends JsStringProperty("err")
    object Ok extends JsDoubleProperty("ok")
    object Namespace extends JsStringProperty("_ns")
    
    object UpdatedExisting extends JsBooleanProperty("updatedExisting")
    object DocumentCount extends JsIntProperty("n")
    object FsyncFilesCount extends JsIntProperty("fsyncFiles")
  }
}

object MongoTools{
  
  def lastError(db:DB) = 
    new JsObject(db.getLastError.asInstanceOf[BasicDBObject])
  
  def checkBatchDetails(db:DB):Int = {
    val details = lastError(db)
    //do some error checking?
    
    //return the document count, or -1 if it wasn't returned
    details(Mongo.Error.DocumentCount,-1)
  }
  
  def generateId():String =
    new com.mongodb.ObjectId().toString()
  
  def marshalId(value:String):Object = {
    val objectId = ObjectId.massageToObjectId(value)
    if(objectId!=null)
      objectId
    else
      value
  }
  
  def marshalDocument(value:DBObject,database:MongoDatabase):Option[JsDocument] = 
    if(value == null)
      None
    else
      Some(new JsDocument(value.asInstanceOf[DBObject],database))

    
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

  /*
  mongo todo:
    gridfs support
    test equality array query operators
    add array specific query operators
    other advanced query operators
    db references?
    support for limit random count group etc for search results
    
  config todo:
  
  
  app todo:
    
  
  */
