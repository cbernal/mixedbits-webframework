package net.mixedbits.mongo

import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._
import com.mongodb._

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
      (db,rawCollection) =>
      rawCollection.update(
        constraint.map(_.buildSearchObject).getOrElse(new BasicDBObject),
        updates.buildUpdateObject,
        false,
        true
      )
      
      MongoTools.checkBatchDetails(db)
    }
    
  def updateFirst(updates:MongoUpdate){
    collection.usingWriteConnection{
      (db,rawCollection) =>
      rawCollection.update(
        constraint.map(_.buildSearchObject).getOrElse(new BasicDBObject),
        updates.buildUpdateObject
      )
      
      MongoTools.checkBatchDetails(db)
    }
  }
}
