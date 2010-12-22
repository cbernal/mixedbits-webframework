package net.mixedbits.tools

import org.bson._
import org.bson.types.BasicBSONList
import scala.collection.JavaConversions._

trait DataFormat[T]{
  def encode(obj:BSONObject):T
  def decode(encoded:T):BSONObject
}

object DataFormat{
  implicit object BSONObjectFormat extends DataFormat[BSONObject]{
    def encode(obj:BSONObject) = obj
    def decode(obj:BSONObject) = obj
  }
  implicit object JsonObjectFormat extends DataFormat[org.json.JSONObject]{
    import org.json._
    def encode(obj:BSONObject):JSONObject = {
      val result = new JSONObject()
      for( key <- obj.keySet; value = obj.get(key) ) value match {
        case value:BasicBSONList => result.put(key,encodeList(value))
        case value:BSONObject => result.put(key,encode(value))
        case value => result.put(key,value)
      }
      result
    }
    def encodeList(list:BasicBSONList):JSONArray = {
      val result = new JSONArray()
      for( value <- list ) value match {
        case value:BasicBSONList => result.put(encodeList(value))
        case value:BSONObject => result.put(encode(value))
        case value => result.put(value)
      }
      result
    }
    def decode(encoded:JSONObject):BSONObject = {
      val result = new BasicBSONObject()
      for( key <- JSONObject.getNames(encoded); value = encoded.get(key) ) value match {
        case value:JSONArray => result.put(key,decodeList(value))
        case value:JSONObject => result.put(key,decode(value))
        case value => result.put(key,value)
      }
      result
    }
    def decodeList(list:JSONArray):BasicBSONList = {
      val result = new BasicBSONList()
      for( i <- 0 until list.length; value = list.get(i)) value match {
        case value:JSONArray => result.add(decodeList(value))
        case value:JSONObject => result.add(decode(value))
        case value => result.add(value)
      }
      result
    }
  }
  
  implicit object XmlElemFormat extends DataFormat[scala.xml.Elem]{
    import scala.xml._
    def encode(obj:BSONObject):Elem = copy(obj)
    
    def copy(src:BSONObject,name:Option[String] = None):Elem = (
      <object>
      {for( key <- src.keySet if key != "#"; value = src.get(key) ) yield value match {
        case value:BasicBSONList => createList(key,value)
        case value:BSONObject => copy(value,Some(key))
        case _ => createValue(key,value)
      }}
      </object>
    ).copy(label=Option(src.get("#")) collect {case s:String => s} orElse name getOrElse "object")

    def createValue(name:String,value:Any):Elem = <value>{value.toString}</value>.copy(label=name)
    
    def createList(name:String,values:BasicBSONList):Elem = (
      <list type="seq">
      {for( value <- values) yield value match {
        case value:BSONObject => copy(value)
        case _ => createValue("value",value)
      }}
      </list>
    ).copy(label=name)
    
    def decode(encoded:Elem):BSONObject = 
      decodeObject(encoded,true)
    
    def decodeObject(encoded:Elem,root:Boolean = false):BSONObject = {
      val obj = if(root) new BasicBSONObject("#",encoded.label) else new BasicBSONObject
      for(element <- encoded.child collect {case e:Elem => e}) element match {
        case Value(text) => obj.put(element.label,text)
        case _ if (element \ "@type" text) == "seq" => obj.put(element.label,decodeList(element))
        case _ => obj.put(element.label,decodeObject(element))
      }
      obj
    }
    
    def decodeList(encoded:Elem):BasicBSONList = {
      val list = new BasicBSONList
      for(element <- encoded.child collect {case e:Elem => e}) element match {
        case Value(text) => list.add(text)
        case _ => list.add(decodeObject(element,true))
      }
      list
    }
    
    object Value{
      def unapply(e:Elem) = 
        if(e.child.size == 1)
          e.child collect {case t:Atom[_] => t.text} headOption
        else
          None
    }
    
  }
  implicit object XmlDomFormat extends DataFormat[org.w3c.dom.Element]{
    import org.w3c.dom._
    def encode(obj:BSONObject):Element = {
      implicit val doc = Xml.createDocument
      doc.appendChild(copy(obj))
      doc.getDocumentElement
    }
    
    def copy(src:BSONObject,name:Option[String] = None)( implicit doc:Document):Element = {
      val node = doc.createElement(Option(src.get("#")) collect {case s:String => s} orElse name getOrElse "object")
      
      for( key <- src.keySet if key != "#"; value = src.get(key)) value match {
        case value:BasicBSONList => node.appendChild(createList(key,value))
        case value:BSONObject => node.appendChild(copy(value,Some(key)))
        case _ => node.appendChild(createValue(key,value))
      }
      
      node
    }
    
    def createValue(name:String,value:Any)(implicit doc:Document):Node = {
      val node = doc.createElement(name)
      node.appendChild(doc.createTextNode(value.toString))
      node
    }
    
    def createList(name:String,values:BasicBSONList)(implicit doc:Document):Node = {
      val node = doc.createElement(name)
      node.setAttribute("type","seq")
      for( value <- values) value match {
        case value:BSONObject => node.appendChild(copy(value))
        case _ => node.appendChild(createValue("value",value))
      }
      node
    }
    
    
    def decode(encoded:Element):BSONObject = 
      decodeObject(encoded,true)
    
    def decodeObject(encoded:Element,root:Boolean = false):BSONObject = {
      val obj = if(root) new BasicBSONObject("#",encoded.getTagName) else new BasicBSONObject
      for(element <- Xml.elements(encoded,"*")) element match {
        case Value(text) => obj.put(element.getTagName,text)
        case _ if element.getAttribute("type") == "seq" => obj.put(element.getTagName,decodeList(element))
        case _ => obj.put(element.getTagName,decodeObject(element))
      }
      obj
    }
    
    def decodeList(encoded:Element):BasicBSONList = {
      val list = new BasicBSONList
      for(element <- Xml.elements(encoded,"*")) element match {
        case Value(text) => list.add(text)
        case _ => list.add(decodeObject(element,true))
      }
      list
    }
    
    object Value{
      def unapply(e:Element) = e.getFirstChild match {
        case text:Text if e.getChildNodes.getLength == 1 =>
          Some(text.getWholeText)
        case _ =>
          None
      }
    }
  }
}
