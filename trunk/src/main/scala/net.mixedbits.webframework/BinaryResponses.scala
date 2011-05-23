package net.mixedbits.webframework


trait BinaryResponse extends WebResponse{
  import java.io.InputStream
  
  def contentType:String
  def contentLength:Option[Int] = None
  def fileName:Option[String] = None
  
  def processRequest(outputStream:java.io.OutputStream):Unit
  
  def processRequest{
   
    WebRequest.httpResponse.setContentType(contentType)
    
    for(value <- fileName)
      WebRequest.httpResponse.setHeader("Content-Disposition","attachment;filename=\""+value+"\";")
    for(value <- contentLength)
      WebRequest.httpResponse.setContentLength(value)
    
    val outputStream = WebRequest.httpResponse.getOutputStream
    try{
      processRequest(outputStream)
    }
    finally{
      outputStream.close()
    }
    
  }
  
}

trait SimpleBinaryResponse extends WebResponse{
  import java.io.InputStream
  
  case class BinaryResponse(contentType:Option[String],fileName:Option[String],contentLength:Option[Int],stream:InputStream)
  
  implicit def tupleToBinaryResponseData(tuple:(String,InputStream)):BinaryResponse = tuple match {
    case (contentType,stream) => BinaryResponse(Some(contentType),None,None,stream)
  }
  implicit def tupleToBinaryResponseData(tuple:(String,Int,InputStream)):BinaryResponse = tuple match {
    case (contentType,contentLength,stream) => BinaryResponse(Some(contentType),None,Some(contentLength),stream)
  }
  implicit def tupleToBinaryResponseData(tuple:(String,String,Int,InputStream)):BinaryResponse = tuple match {
    case (contentType,fileName,contentLength,stream) => BinaryResponse(Some(contentType),Some(fileName),Some(contentLength),stream)
  }  
  
  implicit def toBinaryResponse(stream:InputStream):BinaryResponse = BinaryResponse(None,None,None,stream)
  
  def data:BinaryResponse
  
  def processRequest{
    
    //val (contentType,content) = data
    val response = data;
    val content = response.stream;
    
    WebRequest.httpResponse.setContentType(response.contentType getOrElse "application/octet-stream")
    
    for(value <- response.fileName)
      WebRequest.httpResponse.setHeader("Content-Disposition","attachment;filename=\""+value+"\";")
    for(value <- response.contentLength)
      WebRequest.httpResponse.setContentLength(value)
    
    val outputStream = WebRequest.httpResponse.getOutputStream
    try{
      val buffer = new Array[Byte](1024*16)
      while(true){
        val count = content.read(buffer)
        if(count == -1)
          return
        
        outputStream.write(buffer,0,count)
      }
    } finally {
      try{ outputStream.close() } catch { case e => () }
    }
  }
  
}
