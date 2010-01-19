package net.mixedbits.tools

import Numbers._
import Strings._
import Sequences._
import Files._
import BlockStatements._

object Numbers{
  implicit def stringNumberParsingExtensions(value:String) = new StringNumberParsingExtensions(value)  
}

object Strings{
  implicit def stringExtensions(value:String) = new StringExtensions(value)
}

object Sequences{
  implicit def sequenceExtensions[T](value:Seq[T]) = new SequenceExtensions(value)
  
  def sort[T](list:Array[T])(compareFunction:(T,T) => Boolean):Array[T] = scala.util.Sorting.stableSort(list,compareFunction)
}

object Dates{
  implicit def stringDateParsingExtensions(value:String) = new StringDateParsingExtensions(value)
  
  import java.text.SimpleDateFormat
  import scala.collection.mutable._
  
  private val parsers = Map[String,SimpleDateFormat]()
  
  def parser(format:String):SimpleDateFormat = {
    if(!parsers.contains(format))
      parsers(format) = new SimpleDateFormat(format)

    parsers(format)  
  }
}

object Files{
  import java.io._
  def write(file:File) = new{
    def using[T](f: Writer=>T):T = {
      var writer:Writer = null
      try{
        writer = new FileWriter(file)
        f(writer)
      }
      finally{
        if(writer!=null)
          writer.close
      }
    }
  }
  
  def read(file:File) = new{
    def using[T](f: Reader=>T):T = {
      var reader:Reader = null
      try{
        reader = new FileReader(file)
        f(reader)
      }
      finally{
        if(reader!=null)
          reader.close
      }
    }
  }
  
  def userPath(start:String,remaining:String*):File = path(new File(System.getProperty("user.home"),start),remaining:_*)
  def path(start:String,remaining:String*):File = path(new File(start),remaining:_*)
  def path(start:File,remaining:String*):File = {
    var result = start
    for(part <- remaining)
      result = new File(result,part)
    result    
  }
  

  
  def tempFolder():File = {
    val result = new File(System.getProperty("java.io.tmpdir"))
    result.mkdirs
    result
  }
  
  def tempFolder(name:String):File = {
    val result = new File(tempFolder,name)
    result.mkdirs
    result
  }
}

object Network{
	def hostname() = 
    try{ java.net.InetAddress.getLocalHost.getHostName }
    catch{ case _ => "Unknown" }
}

object Objects{

  type |[A,B] = Either[A,B]
  implicit def toLeft[A,B](a:A):Either[A,B] = Left(a)
  implicit def toRight[A,B](b:B):Either[A,B] = Right(b)
    
  implicit def toOption[T](value:T):Option[T] = 
    if(value == null)
      None                            
    else
      Some(value)

  def objectPath(o:AnyRef) = o.getClass.getSimpleName.split('$') filter{_!=""}
  def simpleClassName(o:AnyRef) = objectPath(o) mkString "."
}


object BlockStatements{
	def attempt[T](f: =>T):Option[T] = 
	  try{ Objects.toOption(f) }
	  catch{ case _ => None }
	
	def default[T](defaultValue:T)(f: =>T):T =
    attempt(f) match {
      case Some(value) => value
      case None => defaultValue
    }    

  def time[T](f: =>T):(Long,T) = {
    val start = System.currentTimeMillis
    
    var errorOccurred = true
    try{
      val result:T = f
      errorOccurred = false
      return (System.currentTimeMillis - start,result)
    }
    finally{
      if(errorOccurred){
        val duration = System.currentTimeMillis - start
        println("An error occurred timing a process, elapsed time: "+(duration/1000.0)+" seconds")
      }
    }
  }
  
  def printDuration[T](f: =>T):T = {
    val start = System.currentTimeMillis
    try{
      return f
    }
    finally{
      val duration = System.currentTimeMillis - start
      println("Process took "+(duration/1000.0)+" seconds") 
    }
  }
  
  
  def thread(block: =>Any) = {
    val t = new Thread(new Runnable{
      def run(){
        block
      }
    })
    t
  }
  
  def daemonThread(block: =>Any) = {
    val t = thread{ block }
    t.setDaemon(true)
    t
  }

  def startThread(block: =>Any) = {
    val t = thread{ block }
    t.start()
    t
  }

}

class StringDateParsingExtensions(value:String){
  def parseDate(format:String) = Dates.parser(format).parse(value)
}

class StringNumberParsingExtensions(val value:String){
  def parseBoolean = try{ Some(value.toBoolean) } catch{ case _ => None }
  def parseByte = try{ Some(value.toByte) } catch{ case _ => None }
  def parseDouble = try{ Some(value.toDouble) } catch{ case _ => None }
  def parseFloat = try{ Some(value.toFloat) } catch{ case _ => None }
  def parseInt = try{ Some(value.toInt) } catch{ case _ => None }
  def parseLong = try{ Some(value.toLong) } catch{ case _ => None }
  def parseShort = try{ Some(value.toShort) } catch{ case _ => None }
  
  def parseBoolean(default:Boolean):Boolean = parseBoolean.getOrElse(default)
  def parseByte(default:Byte):Byte = parseByte.getOrElse(default)
  def parseDouble(default:Double):Double = parseDouble.getOrElse(default)
  def parseFloat(default:Float):Float = parseFloat.getOrElse(default)
  def parseInt(default:Int):Int = parseInt.getOrElse(default)
  def parseLong(default:Long):Long = parseLong.getOrElse(default)
  def parseShort(default:Short):Short = parseShort.getOrElse(default)
}

//object Boolean{def unapply(value:String) = value.parseBoolean}
//object Byte{def unapply(value:String) = value.parseByte}
//object Double{def unapply(value:String) = value.parseDouble}
//object Float{def unapply(value:String) = value.parseFloat}
//object Int{def unapply(value:String) = value.parseInt}
//object Long{def unapply(value:String) = value.parseLong}
//object Short{def unapply(value:String) = value.parseShort}

class StringExtensions(value:String){
  def stripQuotes = {
    val trimmed = value.trim
    if(trimmed.length < 2)
      trimmed
    else (trimmed.first,trimmed.last) match {
      case ('"','"') | ('\'','\'') => trimmed.substring(1,trimmed.length-1).trim
      case _ => trimmed
    }
  }
  
  def segmentAfter(search:String) = parseSegmentAfter(value,search)
  def segmentBefore(search:String) = parseSegmentBefore(value,search)
  def segmentBetween(start:String, end:String) = parseSegmentBefore(parseSegmentAfter(value,start),end)
  
  def isEmpty = value == null || value.trim == ""
  def isNotEmpty = !isEmpty
  
  private def parseSegmentAfter(value:String, search:String) = { val index = value.indexOf(search); value.substring(index+search.length,value.length) }
  private def parseSegmentBefore(value:String, search:String) = { val index = value.indexOf(search); value.substring(0,index) }
}

class SequenceExtensions[T](items:Seq[T]){
  import scala.util.Sorting
  def selectRandom(desiredQuantity:Int):Array[T] = desiredQuantity match {
    case _ if desiredQuantity < 0 => items.toArray
    case _ => Sorting.stableSort(items,(a:T,b:T)=>MiscTools.random.nextBoolean).take(desiredQuantity)
  }
  def selectRandom(minQuantity:Int,maxQuantity:Int):Array[T] = selectRandom(MiscTools.randomInt(minQuantity,maxQuantity)) 
}

private object MiscTools{
  val random = new scala.util.Random
  def randomInt(minValue:Int,maxValue:Int):Int = random.nextInt(maxValue - minValue + 1)+minValue
}

object IO{
  
  import java.io._
  
  def toInputStream(func:OutputStream=>Any):InputStream = {
    val input = new PipedInputStream
    val output = new PipedOutputStream(input)
    startThread{
      try{
        func(output)
      }
      finally{
        output.close
      }
    }
    input
  }
  
  def pipeStream(inputStream:InputStream,outputStream:OutputStream,bufferSize:Int){
		val buffer = new Array[Byte](bufferSize)
		var readCount = inputStream.read(buffer,0,bufferSize)

		while(readCount != -1){
      outputStream.write(buffer,0,readCount)
      readCount = inputStream.read(buffer,0,bufferSize)
    }
  }
  
  def readAllText(inputStream:InputStream):String = 
    readAllText(inputStream,"UTF-8")
  
  def readAllText(inputStream:InputStream,encoding:String):String = {
    val outputStream = new ByteArrayOutputStream
    pipeStream(inputStream,outputStream,1024*16)
    outputStream.toString(encoding)
  }
}

object Hash{
  import java.io._
  
  def md5(string:String):String = 
    md5(string.getBytes)
  
  def md5(bytes:Array[Byte]):String = 
    md5(new ByteArrayInputStream(bytes))
  
	def md5(stream:InputStream):String = {
	
		val buffer = new Array[Byte](1024)
		var readCount = -1
		val digest = java.security.MessageDigest.getInstance("MD5")
	
		do{
			readCount = stream.read(buffer)
			if(readCount > 0){
				digest.update(buffer, 0, readCount)
      }
    }
		while(readCount != -1)
	
		getHexString(digest.digest())
	}
    
	private def getHexString(bytes:Array[Byte]):String = {
		val result = new StringBuilder
		for(currentByte <- bytes){
			result.append(Integer.toString( ( currentByte & 0xff ) + 0x100, 16).substring( 1 ))
    }
		return result.toString()
  }
}

object Passwords{
  import org.mindrot.bcrypt.BCrypt
  def hash(password:String):String = BCrypt.hashpw(password,BCrypt.gensalt)
  //strength is valid from 4 to 31, 31 is strongest, each increment is twice as much work, 10 is the default value
  def hash(password:String,strength:Int):String = BCrypt.hashpw(password,BCrypt.gensalt(strength))
  def areEqual(password:String,hash:String):Boolean = BCrypt.checkpw(password,hash)
}
