package net.mixedbits.media

import net.mixedbits.tools._
import java.io._
import scala.io._
import scala.math._

object ffmpeg extends ((String,InputStream,String,OutputStream,Double=>Unit)=>Unit){
  
  def apply(inputParams:String, inputStream:InputStream, outputParams:String, outputStream:OutputStream,progress:Double=>Unit = x => ()){
  
    val process = Runtime.getRuntime().exec("ffmpeg "+inputParams+" -i - "+outputParams+" -")
    val threads = List(
                    thread{ process.getOutputStream() map {IO.pipeStream(inputStream,_)} },
                    thread{ process.getInputStream() map {IO.pipeStream(_,outputStream)} },
                    thread{
                    var duration = 1.0
                    for(error <- process.getErrorStream();line <- Source.fromInputStream(error).getLines)
                      if(line contains "Duration")
                        duration =  Time.parseDuration(line.dropWhile(_!=':').drop(1).takeWhile(_!=',').trim)
                      else if(line contains "time=")
                        progress(min(duration,((line split "\\s+" filter {_ startsWith "time="} head).drop(5)).parseDouble(0)) / duration)
                    })
    
    threads foreach {_.start}
    threads foreach {_.join}
    if(process.waitFor != 0)
      error("Exited with error code: "+process.exitValue)
  }
}
