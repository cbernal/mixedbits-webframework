package net.mixedbits.webframework

object WebRequest{
  import scala.util.DynamicVariable
  import javax.servlet.{ServletContext,http}
  import javax.servlet.http.{HttpServletRequest,HttpServletResponse}
  import net.mixedbits.tools._
  
  val requestContext = new DynamicVariable[(WebApplication,ServletContext,HttpServletRequest,HttpServletResponse)](null)
  val webPath = new DynamicVariable[WebPathMatch](null)
  
  def param(name:String) = 
    Objects.toOption(httpRequest.getParameter(name)).filter(_!="")
  
  def param(name:String,default:String):String = param(name).getOrElse(default)
  
  def params(name:String):Array[String] = 
    httpRequest.getParameterValues(name)

  def webApplication = requestContext.value._1
  def servletContext = requestContext.value._2
  def httpRequest = requestContext.value._3
  def httpResponse = requestContext.value._4
  def webpath = webPath.value
  def queryString = httpRequest.getQueryString
  def currentPath = Objects.toOption(queryString) match { 
    case Some(query) => webpath.url+"?"+query
    case None => webpath.url
  }

  def hostName = httpRequest.getHeader("host")
}


trait WebRequest{
 
  def param(name:String) = WebRequest.param(name)
  def param(name:String,default:String):String = param(name).getOrElse(default)
  def params(name:String):Array[String] = WebRequest.params(name)
  def webpath = WebRequest.webpath
  def queryString = WebRequest.queryString
  def currentPath = WebRequest.currentPath
  def hostName = WebRequest.hostName
  
  def rawParams = WebRequest.httpRequest.getParameterMap.asInstanceOf[java.util.Map[String,Array[String]]]
  
}


