package net.mixedbits.webframework

trait WebSession[T <: AnyRef]{
  self:WebResponse=>
  
  val sessionProvider:WebSessionProvider[T]

  def createSession(value:T) = sessionProvider.createSession(value)
  def sessionValue:Option[T] = sessionProvider.currentSessionValue
  def hasSession() = sessionProvider.hasCurrentSession
  def destroySession() = sessionProvider.destroyCurrentSession()
  
  onBefore{sessionProvider.onRequestStart}
  onAfter{sessionProvider.onRequestEnd}
}
