package net.mixedbits.webframework

import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.json._
import net.mixedbits.mongo._

trait MongoSessionProvider extends CustomSessionProvider[JsDocument]{
  
  def sessionCollection():MongoCollection
  
  object IsValidSession extends JsBooleanProperty("IsValidSession")
  object OriginalDocumentId extends JsStringProperty("OriginalDocumentId")

  protected def createSessionForValue(value:JsDocument):String = {
    //use mongo generated id to prevent collisions, and use generatedSessionId to help with security
    val id = (JsTools.generateId + generateSessionId).toLowerCase
    //swap our original id out for the session id and mark our session as valid, then store the session
    value(OriginalDocumentId <~ value.id and IsValidSession <~ true and JsDocument.Id <~ id) |> sessionCollection.save
    id
  }
  
  protected def sessionValueForId(id:String):Option[JsDocument] = {
    sessionCollection.findOne(JsDocument.Id == id and IsValidSession == true) match {
      //restore the existing original document id
      case Some(doc) => Some(doc(JsDocument.Id <~ JsTools.marshalId(doc(OriginalDocumentId,""))))
      case None => None
    }
  }
  
  protected def destroySessionForId(id:String){
    sessionCollection.find(JsDocument.Id == id and IsValidSession == true)
                     .updateFirst(IsValidSession <~ false)
  }

}
