package net.mixedbits.webframework

import scala.xml._

abstract class IncludeFile{
  def elements:List[Elem]
}

case class css(filename:String) extends IncludeFile{
  def elements = List(
    <link rel="stylesheet" type="text/css" href={filename} />
  )
}

case class script(filename:String) extends IncludeFile{
  def elements = List(
    <script type="text/javascript" src={filename}></script>
  )
}

//documented at http://code.google.com/apis/ajax/documentation/
case class googleScriptLoader(scripts:(String,String,Option[String])*) extends IncludeFile{
  def elements = List(
    <script type="text/javascript" src="http://www.google.com/jsapi"></script>,
    <script type="text/javascript">
    {
      scripts.map{
        case (script,version,settings) => 
        settings match{
          case Some(value) => "google.load('"+script+"','"+version+"',"+value+");"
          case None => "google.load('"+script+"','"+version+"');"
        }
      }.mkString("\n")
        
    }
    </script>
  )
}

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

trait WebPage extends TextResponse{
  
  def get = contentType -> html
  def post = contentType -> html
  
  def lang = "en_US"
  def charset = "UTF-8"
  def title:String
  
  def include:List[IncludeFile]
  
  def contentType = "text/html; charset="+charset
  
  def html = 
    """<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">"""+
    Xhtml.toXhtml(
      <html xmlns="http://www.w3.org/1999/xhtml" lang={lang} xml:lang={lang}>
        <head>
          <title>{title}</title>
          <meta http-equiv="Content-Type" content={contentType} />
          {
            for(item <- include; element <- item.elements)
              yield element
          }
        </head>
        {body}
      </html>
      ,false,false)
  
  def body:Elem
  
}

