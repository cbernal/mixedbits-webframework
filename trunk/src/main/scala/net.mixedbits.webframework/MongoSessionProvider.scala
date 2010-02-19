package net.mixedbits.webframework

import net.mixedbits.tools._
import net.mixedbits.json._
import net.mixedbits.mongo._

trait MongoSessionProvider extends CustomSessionProvider[JsDocument]{
  
  def sessionCollection():MongoCollection
  
  object IsValidSession extends JsBooleanProperty("IsValidSession")
  object DocumentId extends JsStringProperty("DocumentId")

  protected def createSessionForValue(value:JsDocument):String = {
    val id = generateSessionId()
    value(DocumentId) = value.id
    value(IsValidSession) = true
    value(JsDocument.Id) = id
    sessionCollection.save(value)
    id
  }
  
  protected def sessionValueForId(id:String):Option[JsDocument] = {
    sessionCollection.findOne(JsDocument.Id == id and IsValidSession == true) match {
      case Some(doc) =>
        doc.id = doc(DocumentId,"")
        Some(doc)
      case None => None
    }
  }
  
  protected def destroySessionForId(id:String){
    sessionCollection.find(JsDocument.Id == id and IsValidSession == true)
                     .updateFirst(IsValidSession <~ false)
  }

}
