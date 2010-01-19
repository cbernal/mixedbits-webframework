package net.mixedbits.mongo

import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._
import com.mongodb._

abstract class MongoResultSet[T <: JsDocument](collection:MongoCollection,constraint:Option[MongoConstraint],resultTemplate:Option[JsPropertyGroup],numToSkip:Option[Int],maxResults:Option[Int]) extends Iterable[T]{

  protected def convertRawObject(rawObject:DBObject):T
  protected lazy val cursor:DBCursor = error("not implemented")
  
  protected def constraintToDBObject = 
    constraint.map(_.buildSearchObject).getOrElse(new BasicDBObject)
  
  def count = size
  def totalCount = cursor.count
  def size = {
    val skip = numToSkip.getOrElse(0)
    val total = totalCount - skip
    //if no max results, just return the total, if max results is less than the total, return max results otherwise return the total
    Math.min(maxResults.getOrElse(total),total)
  }
  
  
  
  def elements = new Iterator[T]{
    private lazy val internalIterator = cursor.iterator // cursor
    def next():T = convertRawObject(internalIterator.next)
    def hasNext():Boolean = internalIterator.hasNext
  }

}

trait MongoUpdatableResultSet[T <: JsDocument]{
  self:MongoResultSet[T] =>
  def update(updates:MongoUpdate):Int
  def updateFirst(updates:MongoUpdate):Boolean
  def remove():Any
  
  protected def updateCollection(collection:MongoCollection)(updates:MongoUpdate):Int = 
    collection.usingWriteConnection{
      (db,rawCollection) =>
      rawCollection.update(
        constraintToDBObject,
        updates.buildUpdateObject,
        false,
        true
      )
      
      MongoTools.checkBatchDetails(db)
    }
    
  protected def updateCollectionFirst(collection:MongoCollection)(updates:MongoUpdate):Boolean = 
    collection.usingWriteConnection{
      (db,rawCollection) =>
      rawCollection.update(
        constraintToDBObject,
        updates.buildUpdateObject
      )
      
      MongoTools.checkBatchDetails(db) > 0
    }
}

class MongoCollectionResultSet(collection:MongoCollection,constraint:Option[MongoConstraint],resultTemplate:Option[JsPropertyGroup],numToSkip:Option[Int],maxResults:Option[Int]) extends MongoResultSet[JsDocument](collection,constraint,None,None,None){
  protected def convertRawObject(rawObject:DBObject) = new JsDocument(rawObject,collection.database)
  
  override protected lazy val cursor = {
    collection.usingReadConnection{
      connection =>
      
      var cursor = connection.find(
                    constraintToDBObject,
                    templateToDBObject
                    )
      
      for(value <- numToSkip)
        cursor = cursor.skip(value)      
      for(value <- maxResults)
        cursor = cursor.limit(value)
      
      cursor
    }
  }
  
  protected def templateToDBObject = {
    val result = new BasicDBObject
    for(template <- resultTemplate)
      for(property <- template.properties)
        result.put(property.propertyName,1)
    result
  }
  
  def select(newResultTemplate:JsProperty[_]):MongoCollectionResultSet =
    select(new JsPropertyGroup(newResultTemplate))  
  def select(newResultTemplate:JsPropertyGroup):MongoCollectionResultSet =
    select(toOption(newResultTemplate)) 
  def select(newResultTemplate:Option[JsPropertyGroup]):MongoCollectionResultSet = 
    new MongoCollectionResultSet(collection,constraint,newResultTemplate,numToSkip,maxResults)
  
  def skip(newSkipCount:Int):MongoCollectionResultSet =
    skip(toOption(newSkipCount))
  def skip(newSkipCount:Option[Int]):MongoCollectionResultSet =
    new MongoCollectionResultSet(collection,constraint,resultTemplate,newSkipCount,maxResults)
  
  def limit(newResultsCount:Int):MongoCollectionResultSet =
    limit(toOption(newResultsCount))
  def limit(newResultsCount:Option[Int]):MongoCollectionResultSet =
    new MongoCollectionResultSet(collection,constraint,resultTemplate,numToSkip,newResultsCount)
}

class MongoCollectionUpdateableResultSet(collection:MongoCollection,constraint:Option[MongoConstraint]) extends MongoCollectionResultSet(collection,constraint,None,None,None) with MongoUpdatableResultSet[JsDocument]{
  def update(updates:MongoUpdate):Int =
    updateCollection(collection)(updates)
  
  def updateFirst(updates:MongoUpdate):Boolean =
    updateCollectionFirst(collection)(updates)
    
  def remove() = 
    collection.usingWriteConnection{
      (db,rawCollection) =>
      rawCollection.remove(constraintToDBObject)
      MongoTools.checkBatchDetails(db)
    }
}
