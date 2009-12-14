package net.mixedbits.webframework

trait WebSessionProvider[T]{
  
  //pass a value and return a session id
  def createSession(value:T):String
  
  
  def currentSessionValue():T
  def hasCurrentSession():Boolean
  def currentSessionId():Option[String]
  
  def destroyCurrentSession()
}

trait CustomSessionProvider[T] extends WebSessionProvider[T]{
  val sessionCookieName = "WEBAPPSESSIONID"
  def destroySession(sessionId:String)
//  def createSession(value:String){
//    /*
//    val uniqueId = WebRequest.webApplication.createSession(value)
//    val sessionCookie = new Cookie(WebRequest.webApplication.sessionCookieName,uniqueId)
//    sessionCookie.setMaxAge(-1) // negative numbers indicate non persistent cookies, IE, delete it when the browser is closed
//    WebRequest.httpResponse.addCookie(sessionCookie)
//    */
//  }
//  def destroySession()
//  def currentSessionId:Option[String] = {
//
//    val cookies = WebRequest.httpRequest.getCookies
//    val sessionCookieName = WebRequest.webApplication.sessionCookieName
//    
//    for(cookie <- cookies if(cookie.getName equals sessionCookieName))
//      return Some(cookie.getValue())
//
//    None
//  }
//  
//  def sessionValue:Option[String] = 
//    currentSessionId match {
//      case Some(sessionId) => None //retrieve value based on id here
//      case None => None
//    }
//  
//  def hasSession:Boolean = 
//    sessionValue match {
//      case Some(_) => true
//      case None => false
//    }
}

trait MongoSessionProvider extends CustomSessionProvider[String]{
  def createSession(value:String):String = java.util.UUID.randomUUID().toString();
  def destroySession(sessionId:String){}
  def destroyCurrentSession(){}
  
  def currentSessionValue():String = ""
  def hasCurrentSession():Boolean = false
  def currentSessionId():Option[String] = None
}

trait MemorySessionProvider[T] extends WebSessionProvider[T]{
  
  val sessionValueKey = "MemorySessionProvider:Value" 
  
  def createSession(value:T):String = {
    val servletSession = WebRequest.httpRequest.getSession;
    servletSession.setAttribute(sessionValueKey,value)
    servletSession.getId
  }

  def destroyCurrentSession(){
    val servletSession = WebRequest.httpRequest.getSession
    servletSession.removeAttribute(sessionValueKey)
    servletSession.invalidate()
  }
  
  def hasCurrentSession() = {
    val sessionValue = currentSessionValue
    sessionValue!= null && sessionValue!=""
  }
  
  def currentSessionId():Option[String] = 
    if(hasCurrentSession)
      Some(WebRequest.httpRequest.getSession.getId())
    else
      None
  
  def currentSessionValue():T = { 
    WebRequest.httpRequest.getSession.getAttribute(sessionValueKey).asInstanceOf[T]
  }
}

trait EmptySessionProvider extends WebSessionProvider[Nothing]{
  def createSession(value:Nothing):String = error("EmptySessionProvider: sessions are unsupported.")
  
  
  def currentSessionValue():Nothing = error("EmptySessionProvider: sessions are unsupported.")
  def hasCurrentSession():Boolean = error("EmptySessionProvider: sessions are unsupported.")
  def currentSessionId():Option[String] = error("EmptySessionProvider: sessions are unsupported.")
  
  def destroyCurrentSession() = error("EmptySessionProvider: sessions are unsupported.")
}
