package net.mixedbits.xmlstore

import net.mixedbits.tools._
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

trait XmlConverter[T <: AnyRef]{
  def fromString(s:String):T
  def toDocument(value:T):org.w3c.dom.Document
}
object XmlConverter{
  implicit def dataObjectConverter[T <: DataObject](implicit manifest:ClassManifest[T]) = new XmlConverter[T]{
    def fromString(value:String) = DataObject.load[T,org.w3c.dom.Element](Xml.parse(value))
    def toDocument(value:T) = value.convertTo[org.w3c.dom.Element].getOwnerDocument
  }
  implicit def xstreamConverter[T <: AnyRef](implicit manifest:ClassManifest[T]) = new XmlConverter[T]{
    private lazy val xstream = {
      val x = XStreamConversions(new XStream())
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
    
    def fromString(value:String) = xstream.fromXML(value).asInstanceOf[T]
    def toDocument(value:T) = Xml.parse(xstream.toXML(value)).getOwnerDocument
  }
}

class Collection[T <: AnyRef](specifiedName:String = null)(implicit store:XmlStore, xmlConverter:XmlConverter[T]){
  
  store.register(this.asInstanceOf[Collection[AnyRef]])
  
  sealed trait CollectionOutputFormat[X]{
    def retreive(id:String):X
    def fromRawString(data:String):X
  }
  object CollectionOutputFormat{
    implicit object StringOutputFormat extends CollectionOutputFormat[String]{
      def retreive(id:String):String = store.sqlDatabase{ implicit connection =>
        (for(row <- store._documents.findAll where ('_collection === name and '_id === id))
          yield row[String]('document)).head
      }
      def fromRawString(data:String) = data
    }
    implicit object DomOutputFormat extends CollectionOutputFormat[org.w3c.dom.Document]{
      def retreive(id:String):org.w3c.dom.Document = Xml.parse(StringOutputFormat.retreive(id)).getOwnerDocument
      def fromRawString(data:String) = Xml.parse(data).getOwnerDocument
    }
    implicit object ElemOutputFormat extends CollectionOutputFormat[scala.xml.Elem]{
      def retreive(id:String):scala.xml.Elem = scala.xml.XML.loadString(StringOutputFormat.retreive(id))
      def fromRawString(data:String):scala.xml.Elem = scala.xml.XML.loadString(data)
    }
    implicit object specificOutputFormat extends CollectionOutputFormat[T]{
      def retreive(id:String):T = xmlConverter.fromString(StringOutputFormat.retreive(id))
      def fromRawString(data:String):T = xmlConverter.fromString(data)
    }
  }

  def className = getClass.getSimpleName split '$' filter {_!=""} last
  lazy val name = Option(specifiedName) getOrElse className
  lazy val definition = store.schema.collections.filter(_.name == name).headOption getOrElse error("Unable to find a collection definition for "+name)
  lazy val views = definition.views map {view => new View(store,view,store.viewTables.filter(_.name == view.name).head)}
  lazy val idExtractor = XPathFactory.newInstance().newXPath().compile(definition.documentId)
  
  def store(obj:T):String = {
    store(xmlConverter.toDocument(obj))
  }
  
  def store(stream:InputStream):String = {
    store(Xml.parse(stream).getOwnerDocument)
  }
  
  def store(doc:String):String = {
    store(Xml.parse(doc).getOwnerDocument)
  }
  
  def store(doc:org.w3c.dom.Document):String = store.sqlDatabase{ implicit connection =>
    val id = idExtractor.evaluate(doc)
    
    store._documents.insertOrUpdate(name,id){
      (row,isUpdate) =>
      val now = new java.util.Date
      if(!isUpdate) row('created) = now    
      row('modified) = now
      row('document) = Xml.toFormattedString(doc)
    }
    
    for(view <- views)
      view.remove(name,id)
    
    for(view <- views)
      view.store(name,id,doc)
    
    id
  }
  
  def select(rawSqlQuery:String)(implicit connection:SqlConnection):Iterator[Document] = new Iterator[Document]{
    val statement = connection.rawConnection.createStatement()
    val results = statement.executeQuery(rawSqlQuery)
    val metadata = results.getMetaData()
    val extraColumnNames = (for(i <- 1 to metadata.getColumnCount) yield metadata.getColumnName(i)).toList.filter(name => name != "_collection" && name != "_id")
  
    connection.runOnClose{results.close()}
    connection.runOnClose{statement.close()}
    
    def hasNext = results.hasNext
    def next = Document(store,results.getString("_collection"),results.getString("_id"),extraColumnNames.map(name => Symbol(name) -> results.getString(name)):_*)
  }
  
  def retreive[X:CollectionOutputFormat](id:String):X = 
    implicitly[CollectionOutputFormat[X]].retreive(id)
  
  def retreiveAsString(id:String) = retreive[String](id)
  def retreiveAsDocument(id:String) = retreive[org.w3c.dom.Document](id)
  def retreiveAsElem(id:String) = retreive[scala.xml.Elem](id)
  def retreiveAsObject(id:String) = retreive[T](id)
  
  def loadAsDocument(data:String) = implicitly[CollectionOutputFormat[org.w3c.dom.Document]].fromRawString(data)
  def loadAsElem(data:String) = implicitly[CollectionOutputFormat[scala.xml.Elem]].fromRawString(data)
  def loadAsObject(data:String) = implicitly[CollectionOutputFormat[T]].fromRawString(data)
  
  def remove(id:String) = store.sqlDatabase{ implicit connection =>
    for(view <- views)
      view.remove(name,id)
    store._documents.findAll where ('_collection === name and '_id === id) delete
  }
  
}
