package net.mixedbits.mongo

import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._
import net.mixedbits.json._
import com.mongodb._

import scala.collection.mutable.ListBuffer

trait MongoBaseCollection[T <: JsDocument]{
  def count():Long
  
  def findOne(constraint:JsConstraint):Option[T]
  def findAll():MongoResultSet[T] with MongoUpdatableResultSet[T]
  def find(constraint:JsConstraint):MongoResultSet[T] with MongoUpdatableResultSet[T]
  
  def removeById(id:String):Unit
  def remove(doc:T):Unit
}

class MongoCollection(databaseReference: =>MongoDatabase, name:String, settings:JsObject) extends MongoBaseCollection[JsDocument]{
  type IndexLeft = JsProperty[_]
  type IndexRight = (String,List[JsProperty[_]])
  type IndexParam = IndexLeft|IndexRight
  
  def this(database: => MongoDatabase) = this({database},null,null)
  def this(database: => MongoDatabase,name:String) = this({database},name,null)
  def this(database: => MongoDatabase,settings:JsObject) = this({database},null,settings)
  
  val collectionName:String =
    if(name == null)
      Objects.simpleClassName(this)
    else
      name
    
  private def init(db:MongoDatabase){
    db.withDatabase{
      db =>
      if(settings!=null && !db.getCollectionNames.contains(collectionName))
        db.createCollection(collectionName,settings.obj)
      else
        db.getCollection(collectionName)
    }
  }
    
  lazy val database = {
    val db = databaseReference;
    init(db);
    db
  }
  
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
              //printDuration{
              connection.ensureIndex(indexDescription)
              //}
            
            case Right( (indexName, properties) ) =>
              val indexDescription = new BasicDBObject
              for(property <- properties)
                indexDescription.put(property.propertyName,1)
              //println("Ensuring index("+indexName+"): "+indexDescription)
              //printDuration{
              connection.ensureIndex(indexDescription,indexName)
              //}
              
          }
        }
      }
      catch{
        case e => e.printStackTrace()
      }
    }
  }
  
  def count() = usingReadConnection{_.getCount}
  
  def getAllIds():MongoCollectionResultSet =
    findAll().select(JsAnyProperty("_id"))
  
  def getIds(constraint:JsConstraint):MongoCollectionResultSet =
    find(constraint).select(JsAnyProperty("_id"))
  
  def findAll():MongoCollectionUpdateableResultSet =
    new MongoCollectionUpdateableResultSet(this,None)

  def find(constraint:JsConstraint):MongoCollectionUpdateableResultSet =
    new MongoCollectionUpdateableResultSet(this,constraint)
  
  def findOne:Option[JsDocument] =
    attempt{new JsDocument(usingReadConnection(_.findOne).asInstanceOf[BasicDBObject])}
    
  def findOne(constraint:JsConstraint):Option[JsDocument] =
    usingReadConnection{
      collection=> MongoTools.marshalDocument(collection.findOne(constraint.buildSearchObject))
    }
    
  def getById(id:String):Option[JsDocument] =
    usingReadConnection{
      collection=> MongoTools.marshalDocument(collection.findOne(new JsDocument(id).obj))
    }

  def removeById(id:String){usingWriteConnection{_.remove(new JsDocument(id).obj)}}
  
  def remove(doc:JsDocument) = removeById(doc.id)
  
  def save(doc:JsDocument) = usingWriteConnection{_.save(doc.obj)}
  
  def insert(update:JsUpdate):Boolean = 
    insert(JsTools.generateId())(update)
  
  def insert(id:String)(update:JsUpdate):Boolean = 
    usingWriteConnection{
    (db,rawCollection) =>
    rawCollection.update(
      new JsDocument(id).obj,
      update.buildUpdateObject,
      true,//upsert
      false//multi
      )
      
    MongoTools.checkBatchDetails(db) > 0
    }
  
  def update(doc:JsDocument)(updates:JsUpdate):Boolean =
    update(doc.id)(updates)
  
  def update(id:String)(updates:JsUpdate):Boolean = 
    usingWriteConnection{
    (db,rawCollection) =>
    rawCollection.update(
      new JsDocument(id).obj,
      updates.buildUpdateObject,
      false,//upsert
      false//multi
      )
      
    MongoTools.checkBatchDetails(db) > 0
    }
}
