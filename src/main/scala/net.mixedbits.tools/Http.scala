package net.mixedbits.tools

import Numbers._
import Strings._
import Objects._

import java.io._
import java.net.URLEncoder.encode

import scala.collection.mutable.{Seq => _,_}

import org.apache.commons.httpclient._
import org.apache.commons.httpclient.methods._
import org.apache.commons.httpclient.methods.multipart._

class HttpRequest private[tools](requestMethod:String,url:String){
  
  private sealed abstract class RequestBody
  private case class FileRequestBody(value:File) extends RequestBody
  private case class StringRequestBody(value:String,charset:String) extends RequestBody
  private case class InputStreamRequestBody(value:InputStream,contentLength:Long) extends RequestBody
  private case class FormValueRequestBody(formValues:Seq[(String,String)]) extends RequestBody
  private case class MultipartFormRequestBody(formValues:Seq[(String,Either[String,File])]) extends RequestBody
  //private case class MultipartRequestBody() extends RequestBody
  
  //headers
  private var _contentLength = -1L
  private var _headerValues = new ListBuffer[(String,String)]()
  
  private var hostName:String = null
  
  //forms
  private var _formValues = new ListBuffer[(String,String)]()
  
  //querys
  private var _queryValues = new ListBuffer[(String,String)]()
  
  //context
  private var httpContext = new HttpContext
  
  //request settings
  private var _connectTimeout = -1
  private var _readTimeout = -1
  private var _followRedirects = true
  private var _followMetaRefresh = false
  private var _bufferResponse = false
  
  //request body
  private var requestBody:RequestBody = null

  def header(headerName:String,value:AnyVal):HttpRequest = header(headerName,value.toString)
  def header(header:String,value:String):HttpRequest = {
    
    //find any existing value
    val index = _headerValues.findIndexOf( _._1 == header )
    
    //handle host first, we don't want to add it to the actual list
    if(Http.Host.equalsIgnoreCase(header))
      hostName = value
    //remove value
    else if(value == null && index != -1)
      _headerValues.remove(index)
    //update value
    else if(index != -1)
      _headerValues(index) = (header,value)
    //add value
    else if(value != null)
      _headerValues += header -> value

    //store special headers
    if(Http.ContentLength.equalsIgnoreCase(header))
      _contentLength = value.parseLong getOrElse -1L
    
    this
  }
  
  def contentLength(value:Long) = header(Http.ContentLength,value.toString)
  def contentType(value:String) = header(Http.ContentType,value)
  
  def headerValues(values:(String,String)*) = {_headerValues ++= values; this}
  def formValues(values:(String,String)*) = {_formValues ++= values;requestBody = FormValueRequestBody(_formValues); this}
  def queryValues(values:(String,String)*) = {_queryValues ++= values; this}
  
  def context = httpContext
  def context(newContext:HttpContext) = {httpContext = newContext; this}
  
  
  def followRedirects = _followRedirects
  def followMetaRefresh = _followMetaRefresh
  def bufferResponse = _bufferResponse
  def connectTimeout = _connectTimeout
  def readTimeout = _readTimeout
  
  def followRedirects(value:Boolean) = {_followRedirects = value; this}
  def followMetaRefresh(value:Boolean) = {_followMetaRefresh = value; this}
  def bufferResponse(value:Boolean) = {_bufferResponse = value; this}
  def connectTimeout(value:Int) = {_connectTimeout = value; this}
  def readTimeout(value:Int) = {_readTimeout = value; this}
  
  //body content
  def body(value:File) = {requestBody = FileRequestBody(value); this}
  def body(value:InputStream,contentLength:Long) = {requestBody = InputStreamRequestBody(value,contentLength); this}
  def body(value:String,charset:String) = {requestBody = StringRequestBody(value,charset); this}
  def body(value:String):HttpRequest = body(value,"UTF-8")
  def body(formValues:(String,Either[String,File])*) = {requestBody = MultipartFormRequestBody(formValues); this}

  //responses
  def text = response.text
  def stream = response.stream
  def bufferedStream = response.bufferedStream
  def writeTo(output:OutputStream) = response.writeTo(output)
  def writeTo(file:File) = response.writeTo(file)
  
  def response:HttpResponse = sendRequest(null)
  
  private def sendRequest(requestReason:String):HttpResponse = {
    
    //build our request
    val requestUrl = buildUrl(url,_queryValues:_*)
    val client = new HttpClient
    val method = requestBody match {
      case null => new HttpMethodBase(requestUrl){ def getName:String = requestMethod }
      case _ => new EntityEnclosingMethod(requestUrl){
        def getName:String = requestMethod
        setRequestEntity(
          requestBody match {
            case FileRequestBody(file) => new FileRequestEntity(file,MimeTypes.getContentTypeForName(file.getName))
            case InputStreamRequestBody(inputStream,contentLength) => new InputStreamRequestEntity(inputStream,contentLength)
            case StringRequestBody(value,charset) => new StringRequestEntity(value, "application/x-www-form-urlencoded", charset)
            case FormValueRequestBody(formValues) => new StringRequestEntity(urlEncode(formValues:_*), "application/x-www-form-urlencoded", "UTF-8")
            case MultipartFormRequestBody(formValues) => new MultipartRequestEntity(getMultipartRequestParts(formValues),getParams)
            //case MultipartRequestBody(_) => new MultipartRequestEntity(getMultipartRequestParts(),methodWithRequestBody.getParams())
          }
        )
      }
    }
    
    //we will follow the redirects ourselves, don't let the underlying library do it for us
    method.setFollowRedirects(false)
    
    //set our connect timeout
    if(connectTimeout > 0)
      client.getHttpConnectionManager.getParams.setConnectionTimeout(connectTimeout)
    
    //set our read timeout
    if(readTimeout > 0)
      client.getHttpConnectionManager.getParams.setSoTimeout(readTimeout)
    
    //set our headers
    for( (header,value) <- _headerValues)
      method.setRequestHeader(header,value)
    
    //configure host name
    if(hostName!=null)
      method.getParams.setVirtualHost(hostName)
    
    //configure our client...
    client.getParams.setParameter("http.protocol.single-cookie-header", true)
    
    //send our cookies
    client.setState(context.httpState)
    
    //begin the request
    val responseCode = client.executeMethod(method)
    
    //deal with history
    context.visitUrl(requestUrl,requestReason)
    
    //deal with redirects, 3xx is a redirect
    if(followRedirects && responseCode >= 300 && responseCode < 400){
      return followRedirect(
        method.getResponseHeader("Location").getValue(),
        "redirect("+responseCode+")"
      )
    }
    
    //if we're going to attempt to parse the document, we need to buffer it so we can give it back if necessary...
    if(followMetaRefresh)
    	return searchForMetaRefresh(new HttpResponse(method,true))
    
    return new HttpResponse(method,bufferResponse)
  }
  
  private def urlEncode(items:(String,String)*) = items.map({case(a,b)=>a+"="+encode(b,"UTF-8")}).mkString("&")
  
  private def buildUrl(url:String,items:(String,String)*) = {
    if(items == null || items.isEmpty) url
    else url+(if(url contains "?") "" else "?")+urlEncode(items:_*)
  }

  //maybe we should process the url to deal with relative and rooted urls as well as absolute urls?
  //make sure we pass all necessary settings like context, redirect settings, and meta refresh settings  
  private def followRedirect(url:String, reason:String) = 
    Http.get(url)
      .context(context)
      .followRedirects(followRedirects)
      .followMetaRefresh(followMetaRefresh)
      .sendRequest(reason)
      
  private def searchForMetaRefresh(httpResponse:HttpResponse):HttpResponse = {
    //response body isn't html, don't even look for refresh tags
    if(!httpResponse.contentType.toLowerCase.contains("html"))
      return httpResponse
    
    try{
      val tidy = new org.w3c.tidy.Tidy()
      tidy.setQuiet(true)
      tidy.setShowWarnings(false)
      tidy.setShowErrors(0)
      
      val parsedResponse = tidy.parseDOM(httpResponse.stream,null)
      for(meta <- Xml.elements(parsedResponse,"//meta[@http-equiv]")){
        val key = Xml.text(meta,"@http-equiv")
        val value = Xml.text(meta,"@content")
        
        //parse value, and process redirect
        if("refresh".equalsIgnoreCase(key)){
          return followRedirect(
            value.segmentAfter("=").stripQuotes,
            "meta-refresh"
          )
        }
      }
    }
    catch{
      case e => e.printStackTrace()
    }
    return httpResponse
  }
  
  private def getMultipartRequestParts(formValues:Seq[(String,Either[String,File])]):Array[Part] = {
    formValues.map({
      case (name,value) => value match{
        case Left(stringValue) => new StringPart(name,stringValue)
        case Right(fileValue) => new FilePart(name,fileValue)
      }
    }).toArray
  }
}

class HttpResponse private[tools](method:HttpMethod,bufferResponse:Boolean){
  
  private var bufferedResponse:Array[Byte] = null

  def text = copyStream(stream,new ByteArrayOutputStream,16384).toString(contentEncoding) 
  def writeTo(output:OutputStream){copyStream(stream,output,16384)}
  def writeTo(file:File){
    using(new FileOutputStream(file)){ output => writeTo(output) }
  }
  
  def stream:InputStream = {
    //4xx indicates client error, 5xx indicates server error, there are responseCodes above 5xx
    if(responseCode >= 400)
      throw new java.io.IOException(
        "Server returned HTTP response code: " + responseCode + " " + responseMessage +
          " for URL: " +method.getURI.toString+"\n\n"+
        "Server response body: "+errorText
      )
      
    if(bufferResponse && bufferedResponse==null)
      bufferedResponse = copyStream(method.getResponseBodyAsStream,new ByteArrayOutputStream,16384).toByteArray
    
    if(bufferedResponse!=null)
      return new ByteArrayInputStream(bufferedResponse);
    else
      return method.getResponseBodyAsStream();
  }
  
  def bufferedStream = new BufferedInputStream(stream)
  
	def errorStream = method.getResponseBodyAsStream
  def errorText = copyStream(errorStream,new ByteArrayOutputStream,16384).toString(contentEncoding)
  
  def responseCode = method.getStatusCode
  def responseMessage = method.getStatusLine.getReasonPhrase
  
  def header(header:String) = Option(method.getResponseHeader(header)) map {_.getValue} getOrElse null
  
  def contentType:String = {
    val contentType = header(Http.ContentType)
    if(contentType!=null && contentType.indexOf(";") != -1)
      return contentType.substring(0,contentType.indexOf(";")).trim
		return contentType 
  }
  def contentEncoding = parseEncoding(header(Http.ContentType))
	def contentLength = header(Http.ContentLength).parseLong(-1L)
  def lastModified = try{ java.util.Date.parse(header(Http.LastModified)) } catch{ case _ => 0L }
  
  private def copyStream[A <: OutputStream](input:InputStream,output:A,bufferSize:Int):A = {
    val buffer = new Array[Byte](bufferSize)
    while(true){
      val readCount = input.read(buffer,0,bufferSize)
      if(readCount == -1)
        return output
      output.write(buffer,0,readCount)
    }
    return output
  }
  
  private def using[A <: {def close():Unit},B](param: A)(f:A=>B):B = try{ f(param) } finally{ param.close() }
  
  private def parseEncoding(contentType:String):String = {
    try{
      val start = contentType.indexOf("charset=")
      if(start != -1){
        val semicolon = contentType.indexOf(";",start)
        val end = if(semicolon == -1) contentType.length else semicolon		
        return contentType.substring(start+8,end).replaceAll("\"","").replaceAll("'","").trim
      }
    }
    catch{case _ => ()}
    return "ISO-8859-1"
  }
}

class HttpContext private[tools](){
  private val history = new ListBuffer[(String,String)]
  private[tools] def visitUrl(url:String,reason:String) = history += url -> reason
  private[tools] val httpState = new HttpState;
}

object Http{
  //common headers
  val ContentLength = "Content-Length"
  val ContentType = "Content-Type"
  val LastModified = "Last-Modified"
  val Host = "Host"
  
  //context
  def context = new HttpContext
  
  //common requests
  def request(method:String,url:String) = new HttpRequest(method,url)
  def get(url:String) = new HttpRequest("GET",url)
  def post(url:String) = new HttpRequest("POST",url)
  def put(url:String) = new HttpRequest("PUT",url)
  def delete(url:String) = new HttpRequest("DELETE",url)
  def trace(url:String) = new HttpRequest("TRACE",url)
  def options(url:String) = new HttpRequest("OPTIONS",url)
  def head(url:String) = new HttpRequest("HEAD",url)
}
