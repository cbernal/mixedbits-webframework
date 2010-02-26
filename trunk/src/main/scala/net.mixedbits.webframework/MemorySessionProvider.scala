package net.mixedbits.webframework

import net.mixedbits.tools._

trait MemorySessionProvider[T >: Null <: AnyRef] extends WebSessionProvider[T]{
  
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
  
  def currentSessionValue():Option[T] = 
    Objects.toOption(WebRequest.httpRequest.getSession.getAttribute(sessionValueKey).asInstanceOf[T])
}
