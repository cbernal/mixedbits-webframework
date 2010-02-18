package net.mixedbits.json

import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._
import net.mixedbits.mongo._
import com.mongodb._
import com.mongodb.util._



class JsDocument(baseObject:DBObject,val database:MongoDatabase) extends JsObject(baseObject){
  
  def this() = this(new BasicDBObject,null)
  //def this(baseObject:BasicDBObject) = this(baseObject,null)
  def this(id:String) = {
    this()
    this.id = id
  }
  
  def id:String = toOption(obj.get("_id")).map(_.toString).getOrElse("")
  def id_=(value:String) = obj.put("_id",MongoTools.marshalId(value))
  
  def collection:String = toOption(obj.get("_ns")).map(_.toString).getOrElse("")

}

object JsDocument{
  def apply(updates:MongoUpdate):JsDocument = {
    val doc = new JsDocument
    updates.applyToObject(doc)
    doc
  }
  
  object Id extends JsAnyProperty("_id")
  object Namespace extends JsAnyProperty("_ns")
}
