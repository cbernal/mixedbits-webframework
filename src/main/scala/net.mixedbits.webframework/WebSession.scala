package net.mixedbits.webframework

trait WebSession[T]{
  self:WebResponse=>
  
  val sessionProvider:WebSessionProvider[T]

  def createSession(value:T) = sessionProvider.createSession(value)
  def sessionValue:Option[T] = 
    if(hasSession)
      Some(sessionProvider.currentSessionValue.asInstanceOf[T])
    else
      None
  def hasSession() = sessionProvider.hasCurrentSession
  def destroySession() = sessionProvider.destroyCurrentSession()

}
