package net.mixedbits.config

import java.io._
import java.util._

import net.mixedbits.tools._
import net.mixedbits.tools.Imports._

import scala.reflect.Manifest

class ConfigFile(defaultFile:File,alternateFiles:File*) extends ValNameProvider with DefaultValueExtractorsTrait{
  
  //look in some common places for a file with this name
  def this(filename:String) = this(
    Files.path(filename),
    Files.userPath(".config",filename),
    Files.path("/etc",filename)
  )
  
  val possibleConfigFiles = Array(defaultFile) ++ alternateFiles
  
  //select the first file that exists
  val selectedConfigFile = possibleConfigFiles.filter(_.exists).firstOption 
  
  private lazy val configProperties = 
    for(file <- selectedConfigFile)
      yield loadProperties(file)
  
  private def loadProperties(file:File) =
    for(reader <- new FileReader(file))
      yield {
        val props = new Properties
        props.load(reader)
        props
      }
  
  def property(name:String):Option[String] = 
    for{
      properties <- configProperties
      value <- Objects.toOption(properties.getProperty(name))
    } yield value
      
  class Value[T](defaultValue:Option[T])(implicit extractor:String => Option[T]) extends Provided[T]{

    def this(value:T)(implicit extractor:String => Option[T]) = this(Some(value))
    
    def default = defaultValue getOrElse error("no default")
    
    def name = Objects.objectPath(this).drop(1).mkString(".")
    
    override def notFoundHandler():T = selectedConfigFile match {
      case Some(file) => error("Unable to find value for '"+name+"' in "+file.getAbsolutePath)
      case None => error("Unable to find value for '"+name+"' because no config file was found, the following locations were checked: "+possibleConfigFiles.map(_.getAbsolutePath).mkString("'","','","'"))
    }
    
    override def findDefault:Option[T] = {
      //read the property value, extract the proper type
      val readProperty = property(name)
      val extractedProperty = readProperty.flatMap(extractor)
      
      //use the value from the config, or use the provided default
      for(value <- extractedProperty.orElse(super.findDefault))
        yield value
    }
  }
  
  class ValueForVal[T](defaultValue:Option[T])(implicit extractor:String => Option[T]) extends Value[T](defaultValue){
    def this(value:T)(implicit extractor:String => Option[T]) = this(Some(value))
    override def name = valNameFinder(this).getOrElse(error("unable to determine name for configuredValue"))    
  }
  
  class OptionalValue[T](implicit extractor:String => Option[T]) extends Provided[Option[T]] with NoDefault{
    def name = Objects.objectPath(this).drop(1).mkString(".")
    
    override def notFoundHandler():Option[T] = None
    
    override def findDefault:Option[Option[T]] = {
      //read the property value, extract the proper type 
      property(name).map(extractor)
    }
  }
  
  class OptionalValueForVal[T](implicit extractor:String => Option[T]) extends OptionalValue[T]{
    override def name = valNameFinder(this).getOrElse(error("unable to determine name for configuredValue"))    
  }
  
  def value[T](implicit extractor:String => Option[T]) = new ValueForVal[T](None)
  def valueWithDefault[T](default:T)(implicit extractor:String => Option[T]) = new ValueForVal[T](Objects.toOption(default))
  
  def optional[T](implicit extractor:String => Option[T]) = new OptionalValueForVal[T]
  
}

trait DefaultValueExtractorsTrait{
  implicit def identity[T](value:T):T = value
  implicit def stringToBoolean(value:String) = try{ Some(value.toBoolean) } catch{ case _ => None }
  implicit def stringToInt(value:String) = try{ Some(value.toInt) } catch{ case _ => None }
  implicit def stringToLong(value:String) = try{ Some(value.toLong) } catch{ case _ => None }
  implicit def stringToDouble(value:String) = try{ Some(value.toDouble) } catch{ case _ => None }
}

object DefaultValueExtractors extends DefaultValueExtractorsTrait

/*
object TestConfig extends ConfigFile("testConfig.conf"){
  
  val serverName = value[String]
  
  val timeout = value[Int]
  
  val otherTimeout = valueWithDefault(10)
  
  val optionalValue = optional[Int]
  
  object logging{
    object logFile extends OptionalValue[String]
    object logLevel extends OptionalValue[Int]
    object test extends Value[Int](None)
    object test2 extends Value[Int](Some(10))
  }
  
}
*/
