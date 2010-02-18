package net.mixedbits.mongo

import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.Sequences._
import net.mixedbits.tools.BlockStatements._
import com.mongodb._

sealed abstract class SortDirection(val value:Int)
object SortDirection{
  case object Ascending extends SortDirection(1)
  case object Descending extends SortDirection(-1)
}

abstract class MongoResultSet[T <: JsDocument](collection:MongoCollection,constraint:Option[MongoConstraint]) extends Iterable[T]{

  protected def convertRawObject(rawObject:DBObject):T
  protected lazy val cursor:DBCursor = error("not implemented")
  
  protected def constraintToDBObject = 
    constraint.map(_.buildSearchObject).getOrElse(new BasicDBObject)
  
  def count = size
  def length = size
  def size = totalCount
  def totalCount = cursor.count
  
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
      
      MongoTools.checkBatchDetails(db) == 1
    }
}

class MongoCollectionResultSet(collection:MongoCollection,constraint:Option[MongoConstraint],resultTemplate:Option[JsPropertyGroup],numToSkip:Option[Int],maxResults:Option[Int],sortBy:Seq[(JsProperty[_],SortDirection)]) extends MongoResultSet[JsDocument](collection,constraint){
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
      
      for(value <- prepareSortBy)
        cursor = cursor.sort(value)
      
      cursor
    }
  }
  

  override def size() = {
    val skip = numToSkip.getOrElse(0)
    val total = totalCount - skip
    //if no max results, just return the total, if max results is less than the total, return max results otherwise return the total
    Math.min(maxResults.getOrElse(total),total)
  }
  
  protected def templateToDBObject = {
    resultTemplate.map(propertyGroupToDBObject(_)).getOrElse(new BasicDBObject)
  }
  
  protected def propertyGroupToDBObject(propertyGroup:JsPropertyGroup) = {
    val result = new BasicDBObject
    for(property <- propertyGroup.properties)
      result.put(property.propertyName,1)
    result
  }
  
  protected def prepareSortBy():Option[DBObject] = {
    if(sortBy.size == 0)
      return None

    val result = new BasicDBObject
    for( (property,direction) <- sortBy)
      result.put(property.propertyName,direction.value)
    result
  }
  
  def select(newResultTemplate:JsProperty[_]):MongoCollectionResultSet =
    select(new JsPropertyGroup(newResultTemplate))  
  def select(newResultTemplate:JsPropertyGroup):MongoCollectionResultSet =
    select(toOption(newResultTemplate)) 
  def select(newResultTemplate:Option[JsPropertyGroup]):MongoCollectionResultSet = 
    new MongoCollectionResultSet(collection,constraint,newResultTemplate,numToSkip,maxResults,sortBy)
  
  def skip(newSkipCount:Int):MongoCollectionResultSet =
    skip(toOption(newSkipCount))
  def skip(newSkipCount:Option[Int]):MongoCollectionResultSet =
    new MongoCollectionResultSet(collection,constraint,resultTemplate,newSkipCount,maxResults,sortBy)
  
  def limit(newResultsCount:Int):MongoCollectionResultSet =
    limit(toOption(newResultsCount))
  def limit(newResultsCount:Option[Int]):MongoCollectionResultSet =
    new MongoCollectionResultSet(collection,constraint,resultTemplate,numToSkip,newResultsCount,sortBy)
  
  
  def sortBy(newSortBy:JsProperty[_]):MongoCollectionResultSet =
    sortAscending(newSortBy)
  def sortAscending(newSortBy:JsProperty[_]):MongoCollectionResultSet =
    sortBy(newSortBy,SortDirection.Ascending)
  def sortDescending(newSortBy:JsProperty[_]):MongoCollectionResultSet =
    sortBy(newSortBy,SortDirection.Descending)
  def sortBy(newSortBy:JsProperty[_],direction:SortDirection):MongoCollectionResultSet = 
    new MongoCollectionResultSet(collection,constraint,resultTemplate,numToSkip,maxResults,sortBy ++ List( (newSortBy,direction) ))
    

  def distinct[T](property:JsProperty[T]):Seq[T] = 
    collection.usingReadConnection{
      connection => 
      {
        for(result <- connection.distinct(property.propertyName,constraintToDBObject).iterator)
          yield result.asInstanceOf[T]
      }.toList
    }
    
  def random(maxItems:Int):Iterator[JsDocument] = {
    val totalItems = count.toInt
    if(totalItems <= maxItems)
      elements
    else{
      new Iterator[JsDocument]{
        val indexes = Sequences.randomSet(maxItems,0,totalItems - 1)
        var currentIndex = 0
        def next():JsDocument = {
          val result = skip(indexes(currentIndex)).limit(1).elements.next
          
          currentIndex += 1
          
          result
        }
        def hasNext():Boolean = currentIndex < indexes.length
      }
    }
  }

}

class MongoCollectionUpdateableResultSet(collection:MongoCollection,constraint:Option[MongoConstraint]) extends MongoCollectionResultSet(collection,constraint,None,None,None,Nil) with MongoUpdatableResultSet[JsDocument]{
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