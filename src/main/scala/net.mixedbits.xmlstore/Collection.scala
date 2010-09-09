package net.mixedbits.xmlstore

import net.mixedbits.xmlstore.schema._
import net.mixedbits.sql._

import java.io._

import javax.xml.xpath._
import javax.xml.parsers._
import javax.xml.transform._
import javax.xml.transform.dom._
import javax.xml.transform.stream._

import org.w3c.dom._
import org.xml.sax.InputSource

import com.thoughtworks.xstream.XStream



class Collection[T <: AnyRef](specifiedName:String = null)(implicit store:XmlStore, manifest:ClassManifest[T]){
  
  store.register(this.asInstanceOf[Collection[AnyRef]])
  
  sealed trait CollectionOutputFormat[X]{
    def retreive(id:String):X
  }
  object CollectionOutputFormat{
    implicit object StringOutputFormat extends CollectionOutputFormat[String]{
      def retreive(id:String):String = store.sqlDatabase{ implicit connection =>
        (for(row <- store._documents.findAll where ('_collection === name and '_id === id))
          yield row[String]('document)).head
      }
    }
    implicit object DomOutputFormat extends CollectionOutputFormat[org.w3c.dom.Document]{
      def retreive(id:String):org.w3c.dom.Document = 
        xmlDocumentBuilder.parse(new InputSource(new StringReader(StringOutputFormat.retreive(id))))
    }
    implicit object specificOutputFormat extends CollectionOutputFormat[T]{
      def retreive(id:String):T = xstream.fromXML(StringOutputFormat.retreive(id)).asInstanceOf[T]
    }
  }
  
  private lazy val xmlDocumentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder()
  private lazy val transformerFactory = TransformerFactory.newInstance()
  private lazy val xstream = {
    val x = ScalaConversions(new XStream())
    x.setMode(XStream.NO_REFERENCES)
    
    def findInterestingType(clazz:Class[_]):List[Class[_]] = {
      clazz.getDeclaredMethods.toList.filter(
                          method =>
                          method.getParameterTypes.size==0 &&
                          !((classOf[Product].getDeclaredMethods.map(_.getName).toList ::: "hashCode" :: "toString" :: Nil) contains method.getName) &&
                          !method.getName.contains("copy$default$") &&
                          !method.getReturnType.isPrimitive &&
                          !Array("java.lang","java.util").contains(Option(method.getReturnType.getPackage).map(_.getName).getOrElse("")) &&
                          method.getReturnType!=classOf[Function1[_,_]]
                          ) map {_.getReturnType} map {clazz => var c=clazz; while(c isArray){c = c.getComponentType};c}
    }
    
    x.alias(manifest.erasure.getSimpleName,manifest.erasure)
    
    for(clazz <- findInterestingType(manifest.erasure))
      x.alias(clazz.getSimpleName,clazz)
    
    x
  }

  def className = getClass.getSimpleName split '$' filter {_!=""} last
  lazy val name = Option(specifiedName) getOrElse className
  lazy val definition = store.schema.collections.filter(_.name == name).headOption getOrElse error("Unable to find a collection definition for "+name)
  lazy val views = definition.views map {view => new View(store,view,store.viewTables.filter(_.name == view.name).head)}
  lazy val idExtractor = XPathFactory.newInstance().newXPath().compile(definition.documentId)
  
  def store(obj:T):String = {
    store(xstream.toXML(obj))
  }
  
  def store(stream:InputStream):String = {
    store(xmlDocumentBuilder.parse(stream))
  }
  
  def store(doc:String):String = {
    store(xmlDocumentBuilder.parse(new InputSource(new StringReader(doc))))
  }
  
  def store(doc:org.w3c.dom.Document):String = store.sqlDatabase{ implicit connection =>
    val id = idExtractor.evaluate(doc)
    
    store._documents.insertOrUpdate(name,id){
      (row,isUpdate) =>
      val now = new java.util.Date
      if(!isUpdate) row('created) = now    
      row('modified) = now
      row('document) = nodeToFormattedString(doc)
    }
    
    for(view <- views)
      view.remove(name,id)
    
    for(view <- views)
      view.store(name,id,doc)
    
    id
  }
  
  def retreive[X:CollectionOutputFormat](id:String):X = 
    implicitly[CollectionOutputFormat[X]].retreive(id)
  
  def retreiveAsString(id:String) = retreive[String](id)
  def retreiveAsDocument(id:String) = retreive[org.w3c.dom.Document](id)
  def retreiveAsObject(id:String) = retreive[T](id)
  
  def remove(id:String) = store.sqlDatabase{ implicit connection =>
    for(view <- views)
      view.remove(name,id)
    store._documents.findAll where ('_collection === name and '_id === id) delete
  }

	def nodeToFormattedString(node:Node, indent:Int = 2) = {
    val stringWriter = new StringWriter()
    val transformer = transformerFactory.newTransformer() 
    transformer.setOutputProperty(OutputKeys.INDENT, "yes")
    transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", indent.toString)
    transformer.transform(new DOMSource(node), new StreamResult(stringWriter))
    stringWriter.toString()
	}
  
}
