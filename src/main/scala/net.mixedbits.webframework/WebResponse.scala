package net.mixedbits.webframework

import scala.collection.mutable._

object WebResponseContinueJ2EEProcessing extends Exception
object WebResponseNotFoundException extends Exception
case class WebResponseForwardPage(path:String) extends Exception
case class WebResponseRedirect(redirectType:HttpRedirect,location:String) extends Exception

object WebResponse{
  def continueJ2EEProcessing[T]():T = {throw WebResponseContinueJ2EEProcessing}
  def notFound[T]():T = {throw WebResponseNotFoundException}
  def forward[T](path:String):T = {throw WebResponseForwardPage(path)}
  def redirect[T](redirectType:HttpRedirect,location:String):T = {throw WebResponseRedirect(redirectType,location)}
  def responseCode(code:Int) = 
    WebRequest.httpResponse.setStatus(code)
  
  def responseHeader(name:String,value:String) = 
    WebRequest.httpResponse.setHeader(name,value)
  
}

trait WebResponse{
  
  def processRequest():Unit
  
  def continueJ2EEProcessing[T]():T = WebResponse.continueJ2EEProcessing[T]
  def notFound[T]():T = WebResponse.notFound[T]
  def forward[T](path:String):T = WebResponse.forward[T](path)
  def redirect[T](redirectType:HttpRedirect,location:String):T = WebResponse.redirect[T](redirectType,location)
    
  def httpRequestMethod():String = WebRequest.httpRequest.getMethod
  
  def isHttpPost():Boolean = httpRequestMethod equalsIgnoreCase "POST"
  def isHttpGet():Boolean = httpRequestMethod equalsIgnoreCase "GET"
  
  def responseCode(code:Int) = WebResponse.responseCode(code)
  def responseHeader(name:String,value:String) = WebResponse.responseHeader(name,value)
  
  
  /***********************
  | before/after actions |
  ***********************/
  private val _beforeActions = new ListBuffer[()=>Any]()
  protected def run[T](action: =>T){ _beforeActions += action _ }
  protected def onBefore[T](action: =>T){ _beforeActions += action _ }
  def runBeforeActions(){
    for(action <- _beforeActions)
      action()
  }
  
  private val _afterActions = new ListBuffer[()=>Any]()
  protected def onAfter[T](action: =>T){ _afterActions += action _ }
  def runAfterActions(){
    for(action <- _afterActions)
      action()
  }
}
