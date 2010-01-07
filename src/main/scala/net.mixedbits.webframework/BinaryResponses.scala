package net.mixedbits.webframework


trait BinaryResponse extends WebResponse{
  import java.io.InputStream
  
  //tuple containing contentType and responseBody
  def contentType:String
  
  def processRequest(outputStream:java.io.OutputStream):Unit
  
  def processRequest{
   
    WebRequest.httpResponse.setContentType(contentType)
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
  
  //tuple containing contentType and responseBody
  def data:(String,InputStream)
  
  def processRequest{
    
    val (contentType,content) = data
    
    WebRequest.httpResponse.setContentType(contentType)
    val outputStream = WebRequest.httpResponse.getOutputStream
    try{
      val buffer = new Array[Byte](1024*16)
      while(true){
        val count = content.read(buffer)
        if(count == -1)
          return
        
        outputStream.write(buffer,0,count)
      }
    }
    finally{
      outputStream.close()
    }
  }
  
}
