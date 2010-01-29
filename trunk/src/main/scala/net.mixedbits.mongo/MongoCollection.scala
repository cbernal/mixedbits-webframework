package net.mixedbits.mongo

import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._
import com.mongodb._

import scala.collection.mutable.ListBuffer

trait MongoBaseCollection[T <: JsDocument]{
  def count():Long
  
  def findOne(constraint:MongoConstraint):Option[T]
  def findAll():MongoResultSet[T] with MongoUpdatableResultSet[T]
  def find(constraint:MongoConstraint):MongoResultSet[T] with MongoUpdatableResultSet[T]
  
  def removeById(id:String):Unit
  def remove(doc:T):Unit
}

class MongoCollection(databaseReference: =>MongoDatabase, name:String) extends MongoBaseCollection[JsDocument]{
  type IndexLeft = JsProperty[_]
  type IndexRight = (String,List[JsProperty[_]])
  type IndexParam = IndexLeft|IndexRight
  
  def this(database: => MongoDatabase) = this({database},null)
  
  val collectionName:String =
    if(name == null)
      Objects.simpleClassName(this)
    else
      name
    
  lazy val database = databaseReference
  
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
  
  def getAllIds():MongoCollectionResultSet =
    findAll().select(JsAnyProperty("_id"))
  
  def getIds(constraint:MongoConstraint):MongoCollectionResultSet =
    find(constraint).select(JsAnyProperty("_id"))
  
  def findAll():MongoCollectionUpdateableResultSet =
    new MongoCollectionUpdateableResultSet(this,None)

  def find(constraint:MongoConstraint):MongoCollectionUpdateableResultSet =
    new MongoCollectionUpdateableResultSet(this,constraint)
  
  def findRandom(maxItems:Int):Iterator[JsDocument] = {
    val totalItems = count.toInt
    if(totalItems < maxItems)
      findAll().elements
    else{
      new Iterator[JsDocument]{
        val indexes = Sequences.randomSet(maxItems,0,totalItems)
        var currentIndex = 0
        def next():JsDocument = {
          val result = findAll.skip(indexes(currentIndex)).limit(1).elements.next
          
          currentIndex += 1
          
          result
        }
        def hasNext():Boolean = currentIndex < indexes.length
      }
    }
  }
    
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

  def removeById(id:String){usingWriteConnection{_.remove(new JsDocument(id).obj)}}
  
  def remove(doc:JsDocument) = removeById(doc.id)
  
  def save(doc:JsDocument) = usingWriteConnection{_.save(doc.obj)}
  
  def insert(update:MongoUpdate):Boolean = 
    insert(MongoTools.generateId())(update)
  
  def insert(id:String)(update:MongoUpdate):Boolean = 
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
  
}
