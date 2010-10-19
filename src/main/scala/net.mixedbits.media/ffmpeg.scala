package net.mixedbits.media

import net.mixedbits.tools._
import java.io._
import scala.io._
import scala.math._

object ffmpeg extends ((String,InputStream,String,OutputStream,Double=>Unit)=>Unit){
  
  def apply(inputParams:String, inputStream:InputStream, outputParams:String, outputStream:OutputStream,progress:Double=>Unit = x => ()){
  
    val process = Runtime.getRuntime().exec("ffmpeg "+inputParams+" -i - "+outputParams+" -")
    val inputThread = thread{ process.getOutputStream() map {IO.pipeStream(inputStream,_)} }
    val outputThread = thread{ process.getInputStream() map {IO.pipeStream(_,outputStream)} }
    val errorThread = thread{
      for(error <- process.getErrorStream()){
        
        val lines = Source.fromInputStream(error).getLines
        var duration = 1.0
        for(line <- lines if (line contains "Duration") || (line contains "time="))
          line match {
            case _ if line contains "Duration" =>
              duration =  Time.parseDuration(line.dropWhile(_!=':').drop(1).takeWhile(_!=',').trim)
            case _ =>
              progress(min(duration,((line split "\\s+" filter {_ startsWith "time="} head).drop(5)).parseDouble(0)) / duration)
          }

      }
    }
    
    val threads = List(inputThread, outputThread, errorThread)
    threads foreach {_.start}
    val processExitCode = process.waitFor
    threads foreach {_.join}
    if(processExitCode != 0)
      error("Exited with error code: "+processExitCode)
      
    progress(1)
  }
}
