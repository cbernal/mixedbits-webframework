package net.mixedbits.webframework

import scala.xml._

import Tools._

trait TextResponse extends WebResponse{
  
  //tuple containing contentType and responseBody
  def get:(String,String)
  def post:(String,String)
  
  def processRequest{
    
    val (contentType,content) = httpRequestMethod match {
      case "POST" => post
      case _ => get
    }
    
    WebRequest.httpResponse.setContentType(contentType)
    WebRequest.httpResponse.getWriter.write(content)
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
