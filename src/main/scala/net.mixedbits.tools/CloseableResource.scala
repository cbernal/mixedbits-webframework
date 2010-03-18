package net.mixedbits.tools

import Sequences._
import java.io._

class CloseableResource[T](resource: =>T,onClose: T=>Unit){
  
  def foreach(f: T=>Unit) = map(f)
  
  def map[R](f: T=>R):R = {
    val r = resource
    try{
      f(r)
    }
    finally{
      try{
        if(r!=null)
          onClose(r)
      }
      catch{
        case e =>
          println("error closing resource")
          e.printStackTrace
      }
    }
  }
}

object CloseableResource extends CloseableResourceImports{
  private def usageTest(){
    
    //uses the javaCloseableToCloseableResource conversion
    for(input <- new BufferedReader(new FileReader("/tmp/input.txt"));output <- new FileWriter("/tmp/output.txt") ){
      output.write(input.readLine)
    }
    
    //uses the looksCloseableToCloseableResource conversion
    for(zipFile <- new java.util.zip.ZipFile("/tmp/bla.zip");entry <- zipFile.entries){
      println(entry.getName)
    }
  }
}
trait CloseableResourceImports{
  
  implicit def looksCloseableToCloseableResource[T <: {def close():Unit}](resource:T):CloseableResource[T] = 
    new CloseableResource[T](resource,{_.close})

  implicit def javaCloseableToCloseableResource[T <: java.io.Closeable](resource:T):CloseableResource[T] = 
    new CloseableResource[T](resource,{_.close})
}
