package net.mixedbits.webframework

import javax.servlet._
import javax.servlet.http._
import scala.collection.mutable._

trait WebApplication extends Filter{
  
  val notFoundPage:WebResponse
  //def paths:PartialFunction[String,WebResponse]
  
  type PathMatcher = PartialFunction[String,WebResponse]
  
  private var pathsList:List[PathMatcher] = Nil
  
  def paths(matcher:PathMatcher) = pathsList ::= matcher
  
  private var context:ServletContext = null
  def init(config:FilterConfig){
    context = config.getServletContext
  }
  
  def destroy{
  }
  
	def doFilter(request:ServletRequest, response:ServletResponse, chain:FilterChain){
    val httpRequest = request.asInstanceOf[HttpServletRequest]
    val httpResponse = response.asInstanceOf[HttpServletResponse]
    
    val path = httpRequest.getRequestURI
    
    WebRequest.requestContext.withValue( (this,context,httpRequest,httpResponse) ){
      findPage(path) match {
        case Some(webPage) => return showPage(webPage,path)
        case None => ()
      }      
      
      //detect lack of file and show custom not found page
      if(!new java.io.File(httpRequest.getRealPath(path)).exists){
        
        return showPage(notFoundPage,path)
        
      }

    }
    
    chain.doFilter(request, response)
  }
  
  object Path{
    def unapplySeq(path:String):Option[Seq[String]] = 
      Some(path split '/' drop 1)
  }
  
  def findPage(path:String):Option[WebResponse] = {
    var result:Option[PathMatcher] = None
    for(matcher <- pathsList; if matcher isDefinedAt path)
      result = Some(matcher)
    result map { _(path) }
  }
  
  private def showPage(page:WebResponse,path:String){
    WebRequest.webPath.withValue(WebPathMatch(path)) {
      try{
        page.runBeforeActions()
        page.processRequest()
      }
      catch{
        
        case WebResponseRedirect(redirectType,location) =>
          val HttpRedirect(code) = redirectType
          WebResponse.responseCode(code)
          WebResponse.responseHeader("Location",location)
          
        case WebResponseForwardPage(newPath) =>
          //detect explicit request for different page
          findPage(newPath) match {
            
            //handle it internally
            case Some(webPage) =>
              return showPage(webPage,newPath)
            
            //let the container handle it...
            case None =>
              return context.getRequestDispatcher(newPath)
                            .forward(WebRequest.httpRequest, WebRequest.httpResponse)
            
          }
          
        case e if e == WebResponseNotFoundException => 
          //detect explicit request for not found page
          showPage(notFoundPage,path)

      }
      finally{
        page.runAfterActions()
      }
    }
  }
}


