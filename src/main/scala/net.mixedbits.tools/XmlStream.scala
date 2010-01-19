package net.mixedbits.tools

import scala.xml._
import java.io._
import org.xml.sax._
import org.xml.sax.helpers._
import javax.xml.parsers._


class XmlStream{

  import scala.collection.mutable._   
  
  case class Collector(onCollected: () => Any)
  
  //the sax parser we use depends on the name resolution we are using...
  private lazy val saxParser =
    if(_nameResolver == null)
      SAXParserFactory.newInstance.newSAXParser
    else {
      val factory = SAXParserFactory.newInstance
      factory.setNamespaceAware(true)
      factory.newSAXParser
    }
  
  def parse(element:Elem) { parse(element.toString) }
  def parse(value:String) { parse(value,"UTF-8") }
  def parse(value:String,encoding:String) { parse(new ByteArrayInputStream(value.getBytes(encoding))) }
  def parse(input:InputStream) { saxParser.parse(input,SaxHandler) }
  def parse(file:File) { saxParser.parse(file,SaxHandler) }
  
  
  //name resolution
  var _nameResolver: (String,String,String)=>String = null
  def nameResolver(f:(String,String,String)=>String) = _nameResolver = f
  private def resolveNames(uri:String,localName:String,qName:String) =
    if(_nameResolver!=null)
      _nameResolver(uri,localName,qName)
    else
      qName
    
  //current context
  def select(matcher: PartialFunction[(String,Attributes),Any]) = contextStart(new SelectContext(matcher,null))
  def read(matcher: PartialFunction[(String,Attributes),String=>Any]) = contextStart(new TextContext(matcher,null))
  def collect(matcher:PartialFunction[(String,Attributes),Collector]) = contextStart(new CollectingContext(matcher,null))
  
  
  def onDataCollected(onCollected: => Any) = Collector(onCollected _)
  
  private def contextStart(context:Context){
    currentContext = context
    contextStack.push(context)
  }
  
  private def contextEnd(){
    //remove this context
    contextStack.pop
    //select the new current context
    currentContext = contextStack.peek
    //this wasn't our event, notify the new context
    currentContext.endElement
  }

  private def reset(){ contextStack.clear; currentContext = null }
  private val contextStack = new ArrayStack[Context]
  private var currentContext:Context = null
  
  private abstract class Context{
    def startElement(name:String,attributes:Attributes):Any
    def endElement:Any
    def collect(chars:Array[Char],start:Int,length:Int):Any
  }

  private class SelectContext(matcher:PartialFunction[(String,Attributes),Any],name:String) extends Context{
    val names = new ArrayStack[String]
    var matcherResult:Any = null
    
    //def currentPath = "/"+(names mkString "/")//should be able to use this but ArrayStack iterates in reverse so we have to swap the order :|
    def currentPath = names.foldRight(""){(a,b)=>b+"/"+a} 
    
    def startElement(name:String,attributes:Attributes) = {
      names.push(name)
      if(matcher.isDefinedAt(currentPath,attributes))
        matcherResult = matcher(currentPath,attributes)
      else
        matcherResult = null
    }
      
    def endElement = 
      if(names.isEmpty)
        contextEnd
      else
        names.pop

    def collect(chars:Array[Char],start:Int,length:Int) = ()
  }
  
  private class TextContext(matcher:PartialFunction[(String,Attributes),(String=>Any)],name:String) extends SelectContext(matcher,name){
    var buffer:StringBuilder = null
    
    def onComplete(value: =>String) =
      if(matcherResult!=null){
        matcherResult.asInstanceOf[String=>Any].apply(value)
        //make sure this is nulled out, otherwise the last element will be processed twice...
        matcherResult = null
      }

    override def startElement(name:String,attributes:Attributes) = {
      buffer = new StringBuilder
      super.startElement(name,attributes)
    }
      
    override def endElement = {
      onComplete(buffer.toString)
      super.endElement
    }
    override def collect(chars:Array[Char],start:Int,length:Int){
      if(buffer!=null)
        buffer.append(chars,start,length)
    }
  }
  
  private class CollectingContext(matcher:PartialFunction[(String,Attributes),Collector],name:String) extends SelectContext(matcher,name){
    override def endElement = {
      if(matcherResult!=null){
        matcherResult.asInstanceOf[Collector].onCollected()
        //make sure this is nulled out, otherwise the last element will be processed twice...
        matcherResult = null
      }
        
      super.endElement
    }
  }
    
  object SaxHandler extends DefaultHandler{
    override def startElement(uri:String,localName:String,qName:String,attributes:Attributes) = 
      currentContext.startElement(resolveNames(uri,localName,qName),attributes)
    
    override def endElement(uri:String,localName:String,qName:String) =
      currentContext.endElement
    
    override def characters(chars:Array[Char],start:Int,length:Int) =
      currentContext.collect(chars,start,length)
  }
}


object XmlStream{

  private def createParser(f:(XmlStream)=>Unit):XmlStream = {val parser = new XmlStream; f(parser); parser}
  
  def apply(element:Elem)(f:(XmlStream)=>Unit) { createParser(f).parse(element) }
  def apply(value:String)(f:(XmlStream)=>Unit) { createParser(f).parse(value) }
  def apply(value:String,encoding:String)(f:(XmlStream)=>Unit) { createParser(f).parse(value,encoding) }
  def apply(input:InputStream)(f:(XmlStream)=>Unit) { createParser(f).parse(input) }
  def apply(file:File)(f:(XmlStream)=>Unit) { createParser(f).parse(file) }
  
}
