package net.mixedbits.webframework

import scala.util.DynamicVariable
import net.mixedbits.tools._



case class MultipartField(fieldName:String,value:String)

abstract class MultipartFile{
  def fileSize:Long = -1
  def fieldName:String
  def fileName:String
  def stream:java.io.InputStream
}

case class MultipartStreamingFile(fieldName:String,fileName:String,stream:java.io.InputStream) extends MultipartFile

case class MultipartBufferedFile(fieldName:String,fileName:String,file:java.io.File) extends MultipartFile{
  def stream = new java.io.FileInputStream(file)
  override def fileSize = file.length
}

object StreamingMultipartRequest{
  
  import java.io._
  import org.apache.commons.fileupload._
  import org.apache.commons.fileupload.servlet._
  import org.apache.commons.fileupload.util._
  
  def onMultipartRequest(onFormField: MultipartField=>Any,onFile: MultipartFile=>Any){
    val request = WebRequest.httpRequest
    if(!ServletFileUpload.isMultipartContent(request))
      return
    
    //Parse the request
    val upload = new ServletFileUpload
    val items = upload.getItemIterator(request)
    while(items.hasNext){
      val item = items.next
      val name = item.getFieldName
      val stream = item.openStream()
      if(item.isFormField)
        onFormField(MultipartField(name,Streams.asString(stream)))
      else
        onFile(MultipartStreamingFile(name,item.getName,stream))
    }
  }
}

trait StreamingMultipartRequest{
  import java.io._
  
  def onMultipartRequest(onFormField: MultipartField=>Any,onFile: MultipartFile=>Any) = 
    StreamingMultipartRequest.onMultipartRequest(onFormField,onFile)
}

trait BufferedMultipartRequest{
  self:WebResponse =>
  
  import java.io._
  
  /******************
  | public api      |
  ******************/
  
  def multipartFile(name:String):Option[MultipartFile] = 
    Objects.toOption(multipartParams.value).map{case (_,files) => files}.flatMap(_.get(name))
  
  def multipartParam(name:String):Option[String] = 
    Objects.toOption(multipartParams.value).map{case (fields,_) => fields}.flatMap(_.get(name)).filter(_ != "")
  
  def multipartParam(name:String,default:String):String = 
    multipartParam(name) getOrElse default
  
  
  /******************
  | implementation  |
  ******************/  
  
  private val multipartParams = new DynamicVariable[(Map[String,String],Map[String,MultipartBufferedFile])](null)
  
  private val tempFolderName = Strings.generateGuid
  
  private def buffer(file:MultipartFile):MultipartBufferedFile = {
    val tempFile = Files.tempFile(Objects.className(this),tempFolderName,Strings.generateGuid)
    
    IO.using(new FileOutputStream(tempFile)){ output => 
      IO.pipeStream(file.stream,output)
    }
    
    MultipartBufferedFile(file.fieldName,file.fileName,tempFile)
  }
  
  private def collectMultipartParams():(Map[String,String],Map[String,MultipartBufferedFile]) = {
    import scala.collection.mutable.ArrayBuffer
    
    val formValues = new ArrayBuffer[(String,String)]
    val formFiles = new ArrayBuffer[(String,MultipartBufferedFile)]
    
    StreamingMultipartRequest.onMultipartRequest(
      field => formValues += (field.fieldName,field.value),
      file => formFiles += (file.fieldName,buffer(file))
    )
    
    
    (Map(formValues:_*),Map(formFiles:_*))
  }
  
  onBefore{
    //buffer the values before anyone gets a chance to try to get them
    multipartParams.value = collectMultipartParams()
  }
  
  onAfter{
    //try to clean up after ourselves
    val (_,formFiles) = multipartParams.value
    for(value <- formFiles.values; if value.file.exists)
      if(!value.file.delete())
        value.file.deleteOnExit()
  }
  
}
