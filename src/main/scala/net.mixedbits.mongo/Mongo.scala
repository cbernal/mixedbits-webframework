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
  
  //def removeAll(constraint:MongoConstraint)
  
  def save(doc:JsDocument) = usingWriteConnection{_.save(doc.obj)}
  
}

class JsPropertyGroup{
  
  def this(a:JsProperty[_]) = {
    this()
    this += a
  }
  
  def this(a:JsProperty[_],b:JsProperty[_]) = {
    this()
    this += a
    this += b
  }
  
  protected val _properties = new ListBuffer[JsProperty[_]]
  def properties = _properties.readOnly
  
  def and(property:JsProperty[_]):JsPropertyGroup =
    this += property
  
  def += (property:JsProperty[_]):JsPropertyGroup = {
    _properties += property
    this
  }
}

abstract class MongoUpdate{
  def and(update:MongoUpdate):MongoUpdateGroup = new MongoUpdateGroup(this,update)
  def buildUpdateObject:BasicDBObject = applyToUpdateObject(new BasicDBObject)
  def applyToUpdateObject(obj:BasicDBObject):BasicDBObject
  override def toString = buildUpdateObject.toString
}
      
class MongoUpdateGroup extends MongoUpdate{
  def this(a:MongoUpdate,b:MongoUpdate) = {
    this()
    updates += a
    updates += b
  }
  
  protected val updates = new ListBuffer[MongoUpdate]
  
  override def and(update:MongoUpdate):MongoUpdateGroup =
    this += update
  
  def += (update:MongoUpdate):MongoUpdateGroup = {
    updates += update
    this
  }
  
  def applyToUpdateObject(obj:BasicDBObject):BasicDBObject = {
    for(update <- updates)
      update.applyToUpdateObject(obj)
    obj
  }
}

class MongoPropertyUpdate(key:String,operation:String,value:Any) extends MongoUpdate{
  def applyToUpdateObject(obj:BasicDBObject):BasicDBObject = {
    
    if(obj.containsKey(operation))
      obj.get(operation).asInstanceOf[BasicDBObject].put(key,value)
    else
      obj.put(operation,new BasicDBObject(key,value))

    obj
  }
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

class MongoResultSet(collection:MongoCollection,constraint:Option[MongoConstraint],resultTemplate:Option[JsPropertyGroup],numToSkip:Option[Int],maxResults:Option[Int]) extends Iterable[JsDocument]{

  private def templateToDBObject = {
    val result = new BasicDBObject
    for(template <- resultTemplate)
      for(property <- template.properties)
        result.put(property.propertyName,1)
    result
  }
  
  private lazy val cursor = {
    collection.usingReadConnection{
      connection =>
      
      var cursor = connection.find(
                    constraint.map(_.buildSearchObject).getOrElse(new BasicDBObject),
                    templateToDBObject
                    )
      
      for(value <- numToSkip)
        cursor = cursor.skip(value)      
      for(value <- maxResults)
        cursor = cursor.limit(value)
      
      cursor
    }
  }
  
  def count = size
  def totalCount = cursor.count
  def size = {
    val skip = numToSkip.getOrElse(0)
    val total = totalCount - skip
    //if no max results, just return the total, if max results is less than the total, return max results otherwise return the total
    Math.min(maxResults.getOrElse(total),total)
  }
  
  def elements = new Iterator[JsDocument]{
    val cursorIterator = cursor.iterator
    def next():JsDocument = new JsDocument(cursorIterator.next.asInstanceOf[BasicDBObject],collection.database)
    def hasNext():Boolean = cursorIterator.hasNext
  }
  
  def select(newResultTemplate:JsProperty[_]):MongoResultSet =
    select(new JsPropertyGroup(newResultTemplate))  
  def select(newResultTemplate:JsPropertyGroup):MongoResultSet =
    select(toOption(newResultTemplate)) 
  def select(newResultTemplate:Option[JsPropertyGroup]):MongoResultSet = 
    new MongoResultSet(collection,constraint,newResultTemplate,numToSkip,maxResults)
  
  def skip(newSkipCount:Int):MongoResultSet =
    skip(toOption(newSkipCount))
  def skip(newSkipCount:Option[Int]):MongoResultSet =
    new MongoResultSet(collection,constraint,resultTemplate,newSkipCount,maxResults)
  
  def limit(newResultsCount:Int):MongoResultSet =
    limit(toOption(newResultsCount))
  def limit(newResultsCount:Option[Int]):MongoResultSet =
    new MongoResultSet(collection,constraint,resultTemplate,numToSkip,newResultsCount)
}

class MongoUpdateableResultSet(collection:MongoCollection,constraint:Option[MongoConstraint]) extends MongoResultSet(collection,constraint,None,None,None){
  def update(updates:MongoUpdate) =
    collection.usingWriteConnection{
      _.updateMulti(
        constraint.map(_.buildSearchObject).getOrElse(new BasicDBObject),
        updates.buildUpdateObject
      )
    }
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
