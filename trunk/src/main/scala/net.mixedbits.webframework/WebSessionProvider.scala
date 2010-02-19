package net.mixedbits.webframework

trait WebSessionProvider[T >: Null]{
  
  //pass a value and return a session id
  def createSession(value:T):String
  
  
  def currentSessionValue():Option[T]
  def hasCurrentSession():Boolean
  def currentSessionId():Option[String]
  
  def destroyCurrentSession():Unit
  
  def onRequestStart(){}
  def onRequestEnd(){}
}

trait CustomSessionProvider[T >: Null] extends WebSessionProvider[T]{
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
  
  private val _currentSessionValue = new DynamicVariable[T](null)
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
    val cachedId = _currentSessionId.value
    if(cachedId != null)
      return Some(cachedId)
    
    if(querySessionIdEnabled)
      for(sessionId <- WebRequest.param(sessionCookieName))
        return Some(sessionId)

    val cookies = WebRequest.httpRequest.getCookies
    
    for(cookie <- cookies if(cookie.getName.equals(sessionCookieName) && cookie.getValue!="")){
      _currentSessionId.value = cookie.getValue
      return Some(cookie.getValue)
    }

    None
  }

  def destroyCurrentSession(){
    for(sessionId <- currentSessionId){
      //remove local caches
      _currentSessionValue.value = null
      _currentSessionId.value = null
      
      //remove the value, and expire immediately
      WebRequest.httpResponse.addCookie(sessionCookie("",0))
      
      //request the underlying session store to destroy the session
      destroySessionForId(sessionId)
    }
  }

  def currentSessionValue:Option[T] = {
    val cachedValue = _currentSessionValue.value
    if(cachedValue != null)
      return Some(cachedValue)
    
    currentSessionId match {
      case Some(sessionId) =>
        sessionValueForId(sessionId) match {
          case Some(sessionValue) => 
            _currentSessionValue.value = sessionValue
            Some(sessionValue)
          case None => None
        }
      case None => None
    }
  }
  
  def hasCurrentSession():Boolean = 
    currentSessionValue match { //this will potentially do something expensive, maybe we should use currentSessionId instead...
      case Some(_) => true
      case None => false
    }
    
  override def onRequestEnd(){
    _currentSessionValue.value = null
    _currentSessionId.value = null
  }
}
