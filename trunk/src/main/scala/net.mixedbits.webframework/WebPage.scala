package net.mixedbits.webframework

import scala.xml._

import Tools._

abstract class IncludeFile{
  def elements:Elements
}

case class css(filename:String) extends IncludeFile{
  def elements = 
    <link rel="stylesheet" type="text/css" href={filename} />
}

case class script(filename:String) extends IncludeFile{
  def elements = 
    <script type="text/javascript" src={filename}></script>
}

case class rss(filename:String) extends IncludeFile{
  def elements = 
    <link rel="alternate" type="application/rss+xml" title="RSS" href={filename} />
}

//documented at http://code.google.com/apis/ajax/documentation/
case class googleScriptLoader(scripts:(String,String,Option[String])*) extends IncludeFile{
  def elements = 
    <script type="text/javascript" src="http://www.google.com/jsapi"></script>
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
            for(item <- include)
              yield item.elements
          }
        </head>
        {body}
      </html>
      ,false,false)
  
  def body:Elements
  
}


abstract class SimpleRedirect(redirectType: => HttpRedirect,location: => String) extends WebPage{
  run{
    val HttpRedirect(code) = redirectType
    responseCode(code)
    responseHeader("Location",location)
  }
  def title = "Redirecting to "+location+"..."
  val include = List()
  
  def body = 
    <h1>{title}</h1>
  //  <a href={location}>{location}</a>
}
