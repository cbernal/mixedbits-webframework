package net.mixedbits.webframework

import scala.collection.mutable._

trait WebResponse{
  
  lazy val registeredPages:List[(String,WebResponse)] = registeredPages(new ListBuffer[(String,WebResponse)],_registeredPages.toList).toList
  
  private def registeredPages(results:ListBuffer[(String,WebResponse)],pagesToAdd:List[(String,WebResponse)]):ListBuffer[(String,WebResponse)] = {
    results ++= pagesToAdd
    for( (_,page) <- pagesToAdd)
      registeredPages(results,page.registeredPages)
    results
  }
  private val _registeredPages = new ListBuffer[(String,WebResponse)]()
  
  def registerPages( pages:(String,WebResponse)* ){
    _registeredPages ++= pages
  }
  
  def processRequest:Unit
  
  def httpRequestMethod():String = WebRequest.httpRequest.getMethod
  
  def isHttpPost():Boolean = httpRequestMethod equalsIgnoreCase "POST"
  def isHttpGet():Boolean = httpRequestMethod equalsIgnoreCase "GET"
  
  
  private val _registeredActions = new ListBuffer[()=>Any]()
  protected def run[T](action: =>T){ _registeredActions += action _ }
  def runActions(){
    for(action <- _registeredActions)
      action()
  }
}
