package net.mixedbits.media

import net.mixedbits.tools._
import java.io._
import scala.io._

case class ConversionResult(exitCode:Int,percent:Double,history:Seq[String]){
  def hadError = !hadWarning && exitCode != 0
  def hadWarning = warnings.size > 0
  def throwOnError():this.type = if(hadError) error("ffmpeg exited with error code: "+exitCode+"\n"+history.mkString("\n")) else this
  def throwOnWarning():this.type = if(hadWarning) error("ffmpeg encountered warnings: "+warnings.mkString("\n")) else this
  lazy val warnings = history filter {_ contains "lame: output buffer too small"}
}
object ffmpeg{
  private def handleProgress(process:Process,progress:Double => Unit,debug:String => Unit,threads:Thread*):ConversionResult = {
    var duration = 1.0
    var percentComplete = 0.0
    var lines = List[String]()
    for(error <- use(process.getErrorStream());line <- Source.fromInputStream(error).getLines){
      if(line contains "Duration")
        duration =  Time.parseDuration(line.dropWhile(_!=':').drop(1).takeWhile(_!=',').trim)
      else if(line contains "time=")
        percentComplete = math.min(duration,((line split "\\s+" filter {_ startsWith "time="} head) drop 5).parseDouble(0)) / duration
      lines = (lines drop (if(lines.size >= 10) 1 else 0)) ++ Seq(line)
      debug(line)
      progress(percentComplete)
    }
    threads foreach {_.join}
    progress(1.0)
    ConversionResult(process.waitFor,percentComplete,lines)
  }
  def convertStream(inputStream:InputStream,inputParams:String*)(outputStream:OutputStream,outputParams:String*) = new {
    def apply() = internalConvertStream(inputStream,inputParams,outputStream,outputParams,x=>(),x=>())
    def apply(progress:Double=>Unit) = internalConvertStream(inputStream,inputParams,outputStream,outputParams,progress,x=>())
    def apply(progress:Double=>Unit,debug:String=>Unit) = internalConvertStream(inputStream,inputParams,outputStream,outputParams,progress,debug)
  }
  private def internalConvertStream(inputStream:InputStream,inputParams:Seq[String],outputStream:OutputStream,outputParams:Seq[String],progress:Double=>Unit,debug:String=>Unit):ConversionResult = {
    val process = new ProcessBuilder(List("ffmpeg") ++ inputParams ++ List("-i","-") ++ outputParams ++ List("-") : _*).start()
    handleProgress(process,progress,debug,
      startThread{ use(process.getOutputStream()) foreach {IO.pipeStream(inputStream,_)} },
      startThread{ use(process.getInputStream()) foreach {IO.pipeStream(_,outputStream)} }
    )
  }
  def convertFile(inFile:File,inputParams:String*)(outputStream:OutputStream,outputParams:String*) = new {
    def apply() = internalConvertFile(inFile,inputParams,outputStream,outputParams,x=>(),x=>())
    def apply(progress:Double=>Unit) = internalConvertFile(inFile,inputParams,outputStream,outputParams,progress,x=>())
    def apply(progress:Double=>Unit,debug:String=>Unit) = internalConvertFile(inFile,inputParams,outputStream,outputParams,progress,debug)
  }                 
  private def internalConvertFile(inFile:File,inputParams:Seq[String],outputStream:OutputStream,outputParams:Seq[String],progress:Double=>Unit,debug:String=>Unit):ConversionResult = {
    val process = new ProcessBuilder(List("ffmpeg") ++ inputParams ++ List("-i",inFile.getAbsolutePath) ++ outputParams ++ List("-") : _*).start()
    handleProgress(process,progress,debug,startThread{ use(process.getInputStream()) foreach {IO.pipeStream(_,outputStream)} })
  }
}
