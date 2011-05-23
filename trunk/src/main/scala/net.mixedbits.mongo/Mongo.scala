package net.mixedbits.mongo

import sys.error
import net.mixedbits.tools._
import net.mixedbits.json._
import net.mixedbits.tools._
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
  
  protected def options = {
    val mongoOptions = new MongoOptions
  //  mongoOptions.connectTimeout = 5000 // 5 seconds
  //  mongoOptions.socketTimeout = 5000
  //  mongoOptions.autoConnectRetry = true
    mongoOptions
  }
  
  protected def server(host:String) = 
    new Mongo(new DBAddress(host,"test"),options)
  
  protected def server(host:String,port:Int) = 
    new Mongo(new DBAddress(host,port,"test"),options)
  
  protected def serverPair(leftHost:String,rightHost:String) = 
    new Mongo(new DBAddress(leftHost,"test"),new DBAddress(rightHost,"test"),options)
  
  protected def serverPair(leftHost:(String,Int),rightHost:(String,Int)) = 
    new Mongo(new DBAddress(leftHost._1,leftHost._2,"test"),new DBAddress(rightHost._1,rightHost._2,"test"),options)
}


object Mongo{
  object Error{
    object Message extends JsStringProperty("err")
    object Ok extends JsDoubleProperty("ok")
    object Namespace extends JsStringProperty("_ns")
    
    object UpdatedExisting extends JsBooleanProperty("updatedExisting")
    object DocumentCount extends JsLongProperty("n")
    object FsyncFilesCount extends JsIntProperty("fsyncFiles")
  }
  
  object Collection{
    object Capped extends JsBooleanProperty("capped")
    object ObjectLimit extends JsIntProperty("max")
    object SizeLimit extends JsIntProperty("size")
    
    object AutoIndexId extends JsBooleanProperty("autoIndexId")
  }
}

object MongoTools{
  
  def lastError(db:DB) = 
    new JsObject(db.getLastError.asInstanceOf[BasicDBObject])
  
  def checkBatchDetails(db:DB):Long = {
    val details = lastError(db)
    
    //throw an exception if there is an error message
    for(message <- details(Mongo.Error.Message); if message != "")
      error(details.toJson)
    
    //return the document count, or -1 if it wasn't returned
    details(Mongo.Error.DocumentCount,-1L)
  }
  
  def marshalDocument(value:DBObject):Option[JsDocument] = 
    if(value == null)
      None
    else
      Some(new JsDocument(value))
    
    
  def ensureIndex(collection:DBCollection,indexName:Option[String],properties:(JsProperty[_],SortDirection)*) = {
    val indexDescription = new BasicDBObject
    for( (property,direction) <- properties)
      indexDescription.put(property.propertyName,direction.value)

    if(indexName.isDefined)
      for(value <- indexName)
        collection.ensureIndex(indexDescription,value)
    else
      collection.ensureIndex(indexDescription)
  }
  
}

  /*
  mongo todo:
    test equality array query operators
    add array specific query operators
    other advanced query operators
    db references?
  */
