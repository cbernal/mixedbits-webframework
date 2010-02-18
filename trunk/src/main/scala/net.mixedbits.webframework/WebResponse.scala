package net.mixedbits.webframework

import scala.collection.mutable._

object WebResponseNotFoundException extends Exception
case class WebResponseForwardPage(path:String) extends Exception
case class WebResponseRedirect(redirectType:HttpRedirect,location:String) extends Exception

object WebResponse{
  def notFound[T]():T = {throw WebResponseNotFoundException}
  def forward[T](path:String):T = {throw WebResponseForwardPage(path)}
  def redirect[T](redirectType:HttpRedirect,location:String):T = {throw WebResponseRedirect(redirectType,location)}
  def responseCode(code:Int) = 
    WebRequest.httpResponse.setStatus(code)
  
  def responseHeader(name:String,value:String) = 
    WebRequest.httpResponse.setHeader(name,value)
  
}

trait WebResponse{
  
  def notFound[T]():T = WebResponse.notFound[T]
  def forward[T](path:String):T = WebResponse.forward[T](path)
  def redirect[T](redirectType:HttpRedirect,location:String):T = WebResponse.redirect[T](redirectType,location)
    
  def httpRequestMethod():String = WebRequest.httpRequest.getMethod
  
  def isHttpPost():Boolean = httpRequestMethod equalsIgnoreCase "POST"
  def isHttpGet():Boolean = httpRequestMethod equalsIgnoreCase "GET"
  
  def responseCode(code:Int) = WebResponse.responseCode(code)
  def responseHeader(name:String,value:String) = WebResponse.responseHeader(name,value)
  
  
  
  lazy val registeredPages:List[(String,WebResponse)] = registeredPages(new ListBuffer[(String,WebResponse)],_registeredPages.toList).toList
  
  private def registeredPages(results:ListBuffer[(String,WebResponse)],pagesToAdd:List[(String,WebResponse)]):ListBuffer[(String,WebResponse)] = {
    results ++= pagesToAdd
    for( (_,page) <- pagesToAdd)
      registeredPages(results,page.registeredPages)
    results
  }
  private val _registeredPages = new ListBuffer[(String,WebResponse)]()
  
  protected def registerPages( pages:(String,WebResponse)* ){
    _registeredPages ++= pages
  }
  
  def processRequest():Unit
  
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
