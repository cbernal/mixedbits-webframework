package net.mixedbits.webframework

import javax.servlet._
import javax.servlet.http._
import scala.collection.mutable._

trait WebApplication extends Filter{
  
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
      for( (webPath,webPage) <- registeredPages ){
        val result = webPath testPath path
        result match{
          case Some(webPathMatch) => return showPage(webPage,webPathMatch)
          case None => ()
        }
      }
      
      
      //detect lack of file and show custom not found page
      if(!new java.io.File(httpRequest.getRealPath(path)).exists){
        
        //we aren't in the context of a valid path match anymore, just give it a fake one
        return showPage(notFoundPage,WebPathMatch(path,path))
        
      }
      
      

    }
    
    chain.doFilter(request, response)
  }
  
  private def showPage(page:WebResponse,webPath:WebPathMatch){
    WebRequest.webPath.withValue(webPath) {
      try{
        page.runActions()
        page.processRequest()
      }
      catch{
        case WebResponseRedirect(redirectType,location) =>
          val HttpRedirect(code) = redirectType
          WebResponse.responseCode(code)
          WebResponse.responseHeader("Location",location)
          
        case WebResponseForwardPage(newPath) =>
          //detect explicit request for different page
          context.getRequestDispatcher(newPath).forward(WebRequest.httpRequest, WebRequest.httpResponse)
          
        case e if e == WebResponseNotFoundException => {
          //detect explicit request for not found page
          showPage(notFoundPage,webPath)
          
        }
      }
    }
  }
  

  lazy val registeredPages = Map( (pages ++ pages.flatMap{ case(_,page) => page.registeredPages }):_* ).map{ case(path,page) => WebPath(path) -> page }.toList


  //tuple of path,page
  val pages:Seq[(String,WebResponse)]  
  val notFoundPage:WebResponse

}


