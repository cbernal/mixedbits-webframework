package net.mixedbits.webframework

import javax.servlet.http._
import java.io._

import scala.xml._

trait TextResponse extends WebResponse{
  
  //tuple containing contentType and responseBody
  def get:(String,String)
  def post:(String,String)
  
  def processRequest{    
    writeContent(WebRequest.httpResponse)
  }
  
  protected def writeContent(response:HttpServletResponse){
    val (contentType,content) = httpRequestMethod match {
      case "POST" => post
      case _ => get
    }
    
    response.setContentType(contentType)
    response.getWriter.write(content)
  }
  
}

trait TextResponseWriter extends WebResponse{
  //tuple containing contentType and responseBody
  def get:(String,PrintWriter => Any)
  def post:(String,PrintWriter => Any)
  
  def processRequest{
    writeContent(WebRequest.httpResponse)
  }
  
  protected def writeContent(response:HttpServletResponse){
    val (contentType,handler) = httpRequestMethod match {
      case "POST" => post
      case _ => get
    }
    
    response.setContentType(contentType)
    handler(response.getWriter)
  }
}

trait GZipSupport extends TextResponse{
  
  override def processRequest{
    
    if(isGZipSupported(WebRequest.httpRequest)){
      val wrappedResponse = new com.blogspot.java4it.commons.filters.gzip.GZipResponseWrapper(WebRequest.httpResponse)
      try{
        writeContent(wrappedResponse)
      }
      finally{
        wrappedResponse.finishResponse();
      }
    }
    else{
      super.processRequest()
    }

  }
  
  private def isGZipSupported(req:HttpServletRequest):Boolean = {
    val browserEncodings = req.getHeader("accept-encoding")
    val supported = (browserEncodings != null && browserEncodings.indexOf("gzip") != -1)
    return supported
  }
}

trait PlainTextResponse extends TextResponse{
  def content:String
  def get = "text/plain; charset=UTF-8" -> content
  def post = get 
}

trait XmlResponse extends TextResponse{
  def content:Elem
  def get = ("text/xml; charset=UTF-8", "<?xml version='1.0' encoding='UTF-8'?>\n"+content.toString)
  def post = get 
}

trait JsonResponse extends TextResponse{
  def content:net.mixedbits.json.JsObject
  def get = ("application/json; charset=UTF-8", content.toJson)
  def post = get 
}

trait CssResponse extends TextResponse{
  def content:String
  def get = "text/css; charset=UTF-8" -> content.toString
  def post = get 
}


trait ScriptResponse extends TextResponse{
  def script:String
  def get = "text/javascript" -> script
  def post = get 
}
