package net.mixedbits.mongo

import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._
import com.mongodb._
import com.mongodb.util._


/*
//{"$ref":"collectionName","$id":id}
// or
//{"$ref":"collectionName","$id":id,"$db":"dbName"}

class JsDbRef(database:MongoDatabase,baseRef:DBRef){
  val ref = baseRef
  def this(database:MongoDatabase,collection:String,id:String) = this(database,new DBRef(database.getDatabase,collection,MongoTools.marshalId(id)))
  def this(doc:JsDocument) = this(doc.database,doc.id,doc.collection)
  
  def id:String = ref.getId.toString
  def collection:String = ref.getRef
  
  def fetch() = new JsDocument(ref.fetch().asInstanceOf[BasicDBObject],database)
}
*/
