package net.mixedbits.webframework

import java.io._
import java.util.Properties
import net.mixedbits.tools._

abstract class WebApplicationConfigurationLoader{
  self:WebApplicationLog =>
  
  def applicationName:String
  def configFileName:String = applicationName+".conf"
  
  lazy val searchPaths = 
    Array(
      Files.path(configFileName),
      Files.userPath(".config",configFileName),
      Files.path("/etc",configFileName)
    )
  
  def configFile:Option[File] = {
    for(file <- searchPaths){
      logInfo("Checking config file ["+file.getAbsolutePath+"] "+(if(file.exists) "found" else "missing"))
      if(file.exists)
        return Some(file)
    }
    None
  }
    
  
  def loadConfig:Option[WebApplicationConfiguration] = {
    configFile match {
      case Some(file) => Some(loadConfigFile(file))
      case None => {
        logError("no config file found, can't start application")
        None
      }
    }
  }
  
  def loadConfigFile(file:File):WebApplicationConfiguration = {
    val config = Files.read(file).using{ new WebApplicationConfiguration(_) }
    config.configUrl match {
      case Some(url) => {
        logInfo("Loading config from url: "+url)
        new WebApplicationConfiguration(new StringReader(Http.get(url).text))
      }
      case None => config
    }
  }
}

class ConfigurationProperty[T](defaultValue:T)(implicit manifest:scala.reflect.Manifest[T]){
  def apply(configValue:Option[String]):T =
    configValue match {
      case Some(value) => try{ extractValue(value)(manifest) } catch { case _ => defaultValue }
      case None => defaultValue
    }

  def name = Objects.simpleClassName(this)
  
  protected def extractValue[T](value:String)(typeManifest:scala.reflect.Manifest[T]):T = {
    def isType[T](classReference:Class[T]):Boolean = scala.reflect.Manifest.classType(classReference) <:< typeManifest
    
    if(isType(classOf[String]))
      return value.asInstanceOf[T]
    if(isType(classOf[Int]))
      return value.toInt.asInstanceOf[T]
    if(isType(classOf[Boolean]))
      return value.toBoolean.asInstanceOf[T]
    if(isType(classOf[Long]))
      return value.toLong.asInstanceOf[T]
    if(isType(classOf[Double]))
      return value.toDouble.asInstanceOf[T]
    
    error("Type is not supported: "+manifest)
  }
  
  
}

class OptionConfigurationProperty[T](defaultValue:Option[T])(implicit manifest:scala.reflect.Manifest[T]) extends ConfigurationProperty[Option[T]](defaultValue){
  def this()(implicit manifest:scala.reflect.Manifest[T]) = this(None)
  override def apply(configValue:Option[String]):Option[T] = 
    configValue match {
      case Some(value) => try{ Some(extractValue(value)(manifest)) } catch { case _ => defaultValue }
      case None => defaultValue
    }
}

class WebApplicationConfiguration(reader:Reader){
  
  private val properties = {
    val props = new Properties
    props.load(reader)
    props  
  }

  def configUrl = apply("config.url")
  
  def apply[T](property:ConfigurationProperty[T]):T = property(apply(property.name))
  def apply(property:String):Option[String] = Objects.makeOption(properties.getProperty(property))
}
