package net.mixedbits.tools

import scala.collection.JavaConversions._
import java.io._

class CloseableResource[T](resource: =>T)(implicit onClose: HasClose[T]){
  
  def foreach(f: T=>Unit) = map(f)
  
  def map[R](f: T=>R):R = {
    val r = resource
    try{
      f(r)
    }
    finally{
      try{
        if(r!=null)
          onClose.close(r)
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
    for(input <- use(new BufferedReader(new FileReader("/tmp/input.txt")));output <- use(new FileWriter("/tmp/output.txt")) ){
      output.write(input.readLine)
    }
    
    //uses the looksCloseableToCloseableResource conversion
    for(zipFile <- use(new java.util.zip.ZipFile("/tmp/bla.zip"));entry <- zipFile.entries){
      println(entry.getName)
    }
  }
}

trait HasClose[T]{
  def close(obj:T)
}
object HasClose{
  implicit def javaIoCloseable[T <: java.io.Closeable] = new HasClose[T]{
    def close(obj:T) = obj.close
  }
  
  implicit def looksCloseableToHasClose[T <: {def close():Any}] = new HasClose[T]{
    def close(obj:T) = obj.close
  }
}

trait CloseableResourceImports{
  def use[T:HasClose](resource:T):CloseableResource[T] = new CloseableResource[T](resource)
}
