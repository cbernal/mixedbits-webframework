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

class MongoCollection(val database:MongoDatabase, name:String){
  type IndexLeft = JsProperty[_]
  type IndexRight = (String,List[JsProperty[_]])
  type IndexParam = IndexLeft|IndexRight
  
  def this(database:MongoDatabase) = this(database,null)
  
  val collectionName:String =
    if(name == null)
      Objects.simpleClassName(this)
    else
      name
    
  def usingReadConnection[X](f: DBCollection=>X):X = database.withDatabase(db=>f(db.getCollection(collectionName)))
  def usingWriteConnection[X](f: DBCollection=>X):X = database.withDatabase{
    db=>
    val originalWriteConcern = db.getWriteConcern 
    db.requestStart
    db.setWriteConcern(DB.WriteConcern.STRICT)
    try{
      f(db.getCollection(collectionName))
    }
    finally{
      db.setWriteConcern(originalWriteConcern)
      db.requestDone
    }
  }
  
  def usingWriteConnection[X](f: (DB,DBCollection)=>X):X = database.withDatabase{
    db=>
    val originalWriteConcern = db.getWriteConcern 
    db.requestStart
    db.setWriteConcern(DB.WriteConcern.STRICT)
    try{
      f(db,db.getCollection(collectionName))
    }
    finally{
      db.setWriteConcern(originalWriteConcern)
      db.requestDone
    }
  }
  
  protected def indexId():Unit = usingWriteConnection{_.ensureIDIndex()}
  
  
  protected def indexProperties(indicies:IndexLeft*) =
    index(indicies.map(toLeft[IndexLeft,IndexRight](_)):_*)
  
  protected def indexGroups(indicies:IndexRight*) =
    index(indicies.map(toRight[IndexLeft,IndexRight](_)):_*)
    
  protected def index(indicies:IndexParam*){
    usingWriteConnection{
      connection=>
      
      try{
        for(index <- indicies){
          index match {
            
            case Left(property) =>
              val indexDescription = JsObject(property.propertyName->1).obj
              //println("Ensuring index: "+indexDescription)
              connection.ensureIndex(indexDescription)
            
            
            case Right( (indexName, properties) ) =>
              val indexDescription = new BasicDBObject
              for(property <- properties)
                indexDescription.put(property.propertyName,1)
              //println("Ensuring index("+indexName+"): "+indexDescription)
              connection.ensureIndex(indexDescription,indexName)
              
          }
        }
      }
      catch{
        case e => e.printStackTrace()
      }
    }
  }
  
  def count() = usingReadConnection{_.getCount}
  
  def getAllIds():MongoResultSet =
    findAll().select(JsAnyProperty("_id"))
  
  def getIds(constraint:MongoConstraint):MongoResultSet =
    find(constraint).select(JsAnyProperty("_id"))
  
  def findAll():MongoUpdateableResultSet =
    new MongoUpdateableResultSet(this,None)

  def find(constraint:MongoConstraint):MongoUpdateableResultSet =
    new MongoUpdateableResultSet(this,constraint)
    
  def findOne:Option[JsDocument] =
    attempt{new JsDocument(usingReadConnection(_.findOne).asInstanceOf[BasicDBObject],database)}
    
  def findOne(constraint:MongoConstraint):Option[JsDocument] =
    usingReadConnection{
      collection=> MongoTools.marshalDocument(collection.findOne(constraint.buildSearchObject),database)
    }
    
  def getById(id:String):Option[JsDocument] =
    usingReadConnection{
      collection=> MongoTools.marshalDocument(collection.findOne(new JsDocument(id).obj),database)
    }

  def removeById(id:String) = {usingWriteConnection{_.remove(new JsDocument(id).obj)}}
  
  def remove(doc:JsDocument) = removeById(doc.id)
  
  def removeAll(constraint:MongoConstraint) = 
    usingWriteConnection{
      (db,rawCollection) =>
      rawCollection.remove(constraint.buildSearchObject)
      MongoTools.checkBatchDetails(db)
    }
  
  def save(doc:JsDocument) = usingWriteConnection{_.save(doc.obj)}
  
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
      Some(new JsDocument(value.asInstanceOf[BasicDBObject],database))

    
  def resolveObject(start:BasicDBObject,create:Boolean,fieldPath:Array[String]):BasicDBObject = {
    var currentObject = start
    for(field <- fieldPath){
      if(currentObject.containsField(field)){
        currentObject = currentObject.get(field).asInstanceOf[BasicDBObject]
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
