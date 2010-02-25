package net.mixedbits.webframework

trait WebSessionProvider[T <: AnyRef]{
  
  //pass a value and return a session id
  def createSession(value:T):String
  
  
  def currentSessionValue():Option[T]
  def hasCurrentSession():Boolean
  def currentSessionId():Option[String]
  
  def destroyCurrentSession():Unit
  
  def onRequestStart(){}
  def onRequestEnd(){}
}

trait CustomSessionProvider[T <: AnyRef] extends WebSessionProvider[T]{
  import net.mixedbits.tools._
  import javax.servlet.http.Cookie
  import scala.util.DynamicVariable
  
  val sessionCookieName = "WEBAPPSESSIONID"

  protected def querySessionIdEnabled() = false
  
  protected def sessionValueForId(id:String):Option[T]
  protected def destroySessionForId(id:String):Unit
  protected def createSessionForValue(value:T):String
  
  private lazy val _idGenerator = new net.mixedbits.borrowed.IdGenerator
  protected def generateSessionId():String =
    _idGenerator.generateId(Numbers.randomInt(16,32))
  
  private val _currentSessionValue = new DynamicVariable[T](null.asInstanceOf[T])
  private val _currentSessionId = new DynamicVariable[String](null)
  
  private def sessionCookie(value:String,age:Int):Cookie = {
    val sessionCookie = new Cookie(sessionCookieName,value)
    sessionCookie.setMaxAge(age) // negative numbers indicate non persistent cookies, IE, delete it when the browser is closed
    sessionCookie.setPath("/")
    if(WebRequest.httpRequest.isSecure)
      sessionCookie.setSecure(true)
    sessionCookie
  }
  
  def createSession(value:T):String = {
    val sessionId = createSessionForValue(value)
    
    //cache the values locally
    _currentSessionValue.value = value
    _currentSessionId.value = sessionId
    
    // negative numbers indicate non persistent cookies, IE, delete it when the browser is closed
    WebRequest.httpResponse.addCookie(sessionCookie(sessionId,-1))

    sessionId
  }

  def currentSessionId():Option[String] = {
    //look for id in cache
    val cachedId = _currentSessionId.value
    if(cachedId != null)
      return Some(cachedId)
    
    //look for id in query
    if(querySessionIdEnabled)
      for(sessionId <- WebRequest.param(sessionCookieName))
        return Some(sessionId)
    
    //look for id in cookies
    for(cookie <- WebRequest.cookie(sessionCookieName))
      yield {
        _currentSessionId.value = cookie.getValue
        cookie.getValue
      }
  }

  def destroyCurrentSession(){
    for(sessionId <- currentSessionId){
      //remove local caches
      _currentSessionValue.value = null.asInstanceOf[T]
      _currentSessionId.value = null
      
      //remove the value, and expire immediately
      WebRequest.httpResponse.addCookie(sessionCookie("",0))
      
      //request the underlying session store to destroy the session
      destroySessionForId(sessionId)
    }
  }

  def currentSessionValue:Option[T] = {
    //check cache
    val cachedValue = _currentSessionValue.value
    if(cachedValue != null)
      return Some(cachedValue)
    
    //check session store
    for(sessionId <- currentSessionId; sessionValue <- sessionValueForId(sessionId))
      yield {
        _currentSessionValue.value = sessionValue
        sessionValue
      }
  }
  
  def hasCurrentSession():Boolean = 
    currentSessionValue match {
      case Some(_) => true
      case None => false
    }
    
  override def onRequestEnd(){
    _currentSessionValue.value = null.asInstanceOf[T]
    _currentSessionId.value = null
  }
}
