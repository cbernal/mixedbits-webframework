package net.mixedbits.tools

object MimeTypes{
  def getContentTypeForName(name:String,allowExecutableTypes:Boolean = true) = name.substring(name.lastIndexOf(".")).toLowerCase match{
    case ".html" => "text/html"
    case ".htm" => "text/html"
    case ".txt" => "text/plain"
    case ".css" => "text/css"
    case ".xml" => "text/xml"
    case ".js" if(allowExecutableTypes) => "application/x-javascript"
    case ".jpg" => "image/jpeg"
    case ".jpe" => "image/jpeg"
    case ".jpeg" => "image/jpeg"
    case ".png" => "image/png"
    case ".gif" => "image/gif"
    case ".mp3" => "audio/mpeg"
    case ".swf" if(allowExecutableTypes) => "application/x-shockwave-flash"
    case ".wav" => "audio/x-wav"
    case _ => "application/octet-stream"
  }
}
