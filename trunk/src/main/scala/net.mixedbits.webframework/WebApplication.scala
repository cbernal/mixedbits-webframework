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
    
    for( (webPath,webPage) <- registeredPages ){
      val result = webPath testPath path
      result match{
        case Some(webPathMatch) => {
          WebRequest.requestContext.withValue( (this,context,httpRequest,httpResponse,webPathMatch) ){
            webPage.runActions()
            webPage.processRequest
            return
          }
        }
        case None => ()
      }
      
    }
    
    chain.doFilter(request, response)
  }
  
  //tuple of path,page
  val pages:Seq[(String,WebResponse)]
  lazy val registeredPages = Map( (pages ++ pages.flatMap{ case(_,page) => page.registeredPages }):_* ).map{ case(path,page) => WebPath(path) -> page }.toList
}


