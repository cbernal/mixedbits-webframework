package net.mixedbits.webframework

import scala.xml._
import net.mixedbits.tools._

abstract class IncludeFile{
  def elements:Elements
}

trait WebPage extends TextResponse{
  
  case class css(filename:String) extends IncludeFile{
    def elements = <link rel="stylesheet" type="text/css" href={filename} />
  }
  
  case class script(filename:String) extends IncludeFile{
    def elements = <script type="text/javascript" src={filename}></script>
  }
  
  case class scriptBody(body:String) extends IncludeFile{
    def elements = <script type="text/javascript">{Unparsed(body)}</script>
  }
  
  case class rss(filename:String) extends IncludeFile{
    def elements = <link rel="alternate" type="application/rss+xml" title="RSS" href={filename} />
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
  
  case class unparsedCode(code:String) extends IncludeFile{
    def elements = Elements(Unparsed(code))
  }
  
  
  def get = contentType -> html
  def post = contentType -> html
  
  def lang = "en_US"
  def charset = "UTF-8"
  def title:String
  
  def include:List[IncludeFile] = List()
  
  def contentType = "text/html; charset="+charset
  
  def useStrictDocType = false
  
  def docType = 
    if(useStrictDocType)
      """<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">"""
    else
      """<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">"""
    
  def xmlns = "http://www.w3.org/1999/xhtml"
  
  def html = 
    docType+"\n"+ToXhtml(
      <html xmlns={xmlns} lang={lang} xml:lang={lang}>
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
      )
  
  def body:Elements
  
  def output[T <: AnyRef](content:Option[T]):T = content getOrElse null.asInstanceOf[T]
  
  def outputIf[T <: AnyRef](clause:Boolean)(content: =>T):T = if(clause) content else null.asInstanceOf[T]
  
}


abstract class SimpleRedirect(redirectType: => HttpRedirect,location: => String) extends WebPage{
  run{
    responseCode(redirectType.code)
    responseHeader("Location",location)
  }
  def title = "Redirecting to "+location+"..."
  
  def body = 
    <h1>{title}</h1>
    <a href={location}>{location}</a>
}
