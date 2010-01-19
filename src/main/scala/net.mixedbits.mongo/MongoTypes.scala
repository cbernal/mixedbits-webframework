package net.mixedbits.mongo

import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._
import com.mongodb._
import com.mongodb.util._

class JsObject(baseObject:DBObject){
  def this() = this(new BasicDBObject)
  def add(firstValue:(String,Any),values:(String,Any)*) = {
    update(firstValue._1,firstValue._2)
    for( (key,value) <- values)
      update(key,value)
    this
  }
  
  val obj:DBObject = baseObject
  
  def apply(updates:MongoUpdate):this.type = {
    updates.applyToObject(this)
    this
  }
  
  def apply[T](property:JsProperty[T]):Option[T] =
    property.readValue(obj)
  def apply[T](property:JsProperty[T],defaultValue:T):T =
    property.readValue(obj).getOrElse(defaultValue)
  
  def update[T](property:JsProperty[T],value:T):this.type = {
    property.updateValue(obj,value)
    this
  }
  def update[T](property:JsProperty[T],value:Option[T]):this.type = {
    value match {
      case Some(v) => update(property,v)
      case None => property.removeValue(obj)
    }
    this
  }
  def update[T](propertyName:String,value:T):this.type = {
    obj.put(propertyName,MongoTools.rawValue(value))
    this
  }

  def apply[T](property:JsArrayProperty[T]):JsArray[T] = {
    property.readUncheckedValue(obj) match {
      case Some(list) => new JsArray[T](list)
      case None => {
        val result = new JsArray[T]
        property.putUncheckedValue(obj,result.list)
        result
      }
    }
  }
  def update[T](property:JsArrayProperty[T],value:JsArray[T]):this.type = {
    property.putUncheckedValue(obj,value.list)
    this
  }
  
  def toJson():String = obj.toString
  override def toString = toJson
}

object JsObject{
  def apply(updates:MongoUpdate):JsObject = 
    updates.applyToObject(new JsObject)
  
  def apply() = new JsObject
  def apply(firstValue:(String,Any),values:(String,Any)*) = new JsObject().add(firstValue,values:_*)
  
  def parse(data:String) = new JsObject(JSON.parse(data).asInstanceOf[BasicDBObject])
}

class JsDocument(baseObject:DBObject,val database:MongoDatabase) extends JsObject(baseObject){
  
  def this() = this(new BasicDBObject,null)
  //def this(baseObject:BasicDBObject) = this(baseObject,null)
  def this(id:String) = {
    this()
    this.id = id
  }
  
  def id:String = obj.get("_id").toString
  def id_=(value:String) = obj.put("_id",MongoTools.marshalId(value))
  
  def collection:String = obj.get("_ns").toString
}

object JsDocument{
  def apply(updates:MongoUpdate):JsDocument ={
    val doc = new JsDocument
    updates.applyToObject(doc)
    doc
  }
  
  object Id extends JsAnyProperty("_id")
  object Namespace extends JsAnyProperty("_ns")
}

class JsDbRef(database:MongoDatabase,baseRef:DBRef){
  val ref = baseRef
  def this(database:MongoDatabase,collection:String,id:String) = this(database,new DBRef(database.getDatabase,collection,MongoTools.marshalId(id)))
  def this(doc:JsDocument) = this(doc.database,doc.id,doc.collection)
  
  def id:String = ref.getId.toString
  def collection:String = ref.getRef
  
  def fetch() = new JsDocument(ref.fetch().asInstanceOf[BasicDBObject],database)
}

class JsArray[T](val list:BasicDBList) extends Seq[T]{
  def this() = this(new BasicDBList)
  
  def length = list.size
  def elements = new Iterator[T]{
    private var currentIndex = 0 
    def next():T = {currentIndex+=1;apply(currentIndex-1)}
    def hasNext():Boolean = currentIndex < list.size
  }
  def apply(i:Int):T = {
    val item = list.get(i)
    if(item.isInstanceOf[BasicDBList])
      new JsArray[Any](item.asInstanceOf[BasicDBList]).asInstanceOf[T]
    else if(item.isInstanceOf[BasicDBObject])
      new JsObject(item.asInstanceOf[BasicDBObject]).asInstanceOf[T]
    else
      item.asInstanceOf[T]
  }
  def update(i:Int,value:T) = list.put(i,value)
  def add(firstValue:T,values:T*) = {
    this += firstValue
    
    addAll(values)
  }
  
  def addAll(values:Seq[T]) = this ++= values
  
  def ++= (values:Seq[T]) = {
    for(value <- values)
      this += value
    
    this
  }
  
  def --= (values:Seq[T]) = {
    for(value <- values)
      this -= value
    
    this
  }
  
  def +=(value:T) =
    if(value.isInstanceOf[JsObject])
      list.add(value.asInstanceOf[JsObject].obj)
    else if(value.isInstanceOf[JsArray[_]])
      list.add(value.asInstanceOf[JsArray[_]].list)
    else
      list.add(value.asInstanceOf[AnyRef])

  def -=(value:T){ while(list contains value){list.remove(value)} }
  
  def clear = list.clear
  
  def toJson():String = list.toString
}
object JsArray{
  def apply[T](firstValue:T,values:T*) = new JsArray[T].add(firstValue,values:_*)
  
  def apply[T](values:Seq[T]) = new JsArray[T].addAll(values)
}
