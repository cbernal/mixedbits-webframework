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

  def getDatabase = getConnection.getDB(databaseName)
  def withDatabase[T](f: DB=>T):T = f(getDatabase)
  
  def getConnection:Mongo
}

class MongoCollection(database:MongoDatabase, name:String){
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
  
  protected def indexId():Unit = usingWriteConnection{_.ensureIDIndex()}
  
  
  protected def indexProperties(indicies:IndexLeft*) =
    index(indicies.map(makeLeft[IndexLeft,IndexRight](_)):_*)
  
  protected def indexGroups(indicies:IndexRight*) =
    index(indicies.map(makeRight[IndexLeft,IndexRight](_)):_*)
    
  protected def index(indicies:IndexParam*){
    usingWriteConnection{
      connection=>
      
      try{
        for(index <- indicies){
          index match {
            
            case Left(property) =>
              val indexDescription = JsObject(property.propertyName->1).obj
              println("Ensuring index: "+indexDescription)
              connection.ensureIndex(indexDescription)
            
            
            case Right( (indexName, properties) ) =>
              val indexDescription = new BasicDBObject
              for(property <- properties)
                indexDescription.put(property.propertyName,1)
              println("Ensuring index("+indexName+"): "+indexDescription)
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
  
  def getIds(constraint:MongoConstraint):MongoResultSet =
    find(constraint,JsObject("_id"->1))
  
  def findAll():MongoResultSet = 
    new MongoResultSet(usingReadConnection{_.find()},database)
  
  def find(constraint:MongoConstraint):MongoResultSet = 
    new MongoResultSet(usingReadConnection{_.find(constraint.buildSearchObject)},database)
    
  def find(constraint:MongoConstraint,resultTemplate:JsObject):MongoResultSet = 
    new MongoResultSet(usingReadConnection{_.find(constraint.buildSearchObject, resultTemplate.obj)},database)
    
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
  
  def save(doc:JsDocument) = usingWriteConnection{_.save(doc.obj)} 
  
}



abstract class MongoConstraint{
  def and(constraint:MongoConstraint):MongoConstraintGroup = new MongoConstraintGroup(this,constraint)
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
  
  override def and(constraint:MongoConstraint):MongoConstraintGroup =
    this += constraint
  
  def += (constraint:MongoConstraint):MongoConstraintGroup = {
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

class MongoResultSet(cursor:DBCursor,database:MongoDatabase) extends Iterable[JsDocument]{
  def size = cursor.count
  def elements = new Iterator[JsDocument]{
    val cursorIterator = cursor.iterator
    def next():JsDocument = new JsDocument(cursorIterator.next.asInstanceOf[BasicDBObject],database)
    def hasNext():Boolean = cursorIterator.hasNext
  }
  
  def skip(numToSkip:Int) = 
    new MongoResultSet(cursor.skip(numToSkip),database)
  
  def limit(maxResults:Int) =
    new MongoResultSet(cursor.limit(maxResults),database)
}


object MongoTools{
  
  def marshalId(value:String):Object = {
    val objectId = ObjectId.massageToObjectId(value)
    if(objectId!=null)
      objectId
    else
      value
  }
  
//  def marshalDocument(value:DBObject):Option[JsDocument] = marshalDocument(value,null) 
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
