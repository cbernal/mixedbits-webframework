package net.mixedbits.webframework

import net.mixedbits.tools._
import net.mixedbits.json._
import net.mixedbits.mongo._
import org.scala_tools.time.Imports._

object MongoSessionProvider{
  object IsValidSession extends JsBooleanProperty("IsValidSession")
  object OriginalDocumentId extends JsStringProperty("OriginalDocumentId")
  object SessionExpiration extends JsDateProperty("SessionExpiration")
}

trait MongoSessionProvider extends CustomSessionProvider[JsDocument]{
  import MongoSessionProvider._
  
  def sessionCollection():MongoCollection
  def sessionReValidatePeriod():Duration = 30.minutes
  def sessionDuration():Duration = 2.hours
  
  private def newExpirationDate() = (DateTime.now + sessionDuration).toDate

  protected def createSessionForValue(value:JsDocument):String = {
    //use mongo generated id to prevent collisions, and use generatedSessionId to help with security
    val id = (JsTools.generateId + generateSessionId).toLowerCase
    //swap our original id out for the session id and mark our session as valid, then store the session
    value(
      OriginalDocumentId <~ value.id and
      IsValidSession <~ true and
      SessionExpiration <~ newExpirationDate and
      JsDocument.Id <~ id
      ) |> sessionCollection.save
      
    id
  }
  
  protected def sessionValueForId(id:String):Option[JsDocument] = {
    sessionCollection.findOne(JsDocument.Id == id and IsValidSession == true and SessionExpiration > DateTime.now.toDate) match {
      
      case Some(doc) =>

        //check to see if we should revalidate our session timeout
        for(
          expiration <- doc(SessionExpiration);
          //this can throw an exception if the times are out of order, so default to 0 millis if there is an error
          timeUntilExpiration = default(0.millis.toDuration){(DateTime.now to new DateTime(expiration)).toDuration};
          if timeUntilExpiration < sessionReValidatePeriod
          ) sessionCollection.update(doc){SessionExpiration <~ newExpirationDate}
      
        //restore existing original document id so that the document appears mostly unchanged
        Some(doc(JsDocument.Id <~ JsTools.marshalId(doc(OriginalDocumentId,""))))
        
      case None =>
        None
    }
  }
  
  protected def destroySessionForId(id:String){
    sessionCollection.find(JsDocument.Id == id and IsValidSession == true)
                     .updateFirst(IsValidSession <~ false)
  }

}
