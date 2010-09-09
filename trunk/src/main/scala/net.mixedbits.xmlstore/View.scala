package net.mixedbits.xmlstore

import net.mixedbits.xmlstore.schema._
import net.mixedbits.sql._

import javax.xml.xpath._

class View(store:XmlStore,val definition:DocumentView,val table:SqlTable) extends Iterable[Document]{
  
  val name = definition.name
  
  lazy val extractors = {
    val factory = XPathFactory.newInstance()
    for(column <- definition.columns)
      yield (column.name,factory.newXPath().compile(column.select))
  }
  
  lazy val entrySelector = 
    XPathFactory.newInstance().newXPath.compile(definition.select)
  
  def store(collection:String,id:String,doc:org.w3c.dom.Document)(implicit connection:SqlWriteConnection) = {
    //remove(connection,collection,id)
    
    val nodes = entrySelector.evaluate(doc,XPathConstants.NODESET).asInstanceOf[org.w3c.dom.NodeList]
    for(i <- 0 until nodes.getLength;node = nodes.item(i)){
      table.insert{ row =>
        row('_collection) = collection
        row('_id) = id
        for( (name,xpath) <- extractors )
          row(Symbol(name)) = xpath.evaluate(node)
      }
    }
  }
  
  def remove(collection:String,id:String)(implicit connection:SqlWriteConnection) = 
    table.findAll where ('_collection === collection and '_id === id) delete
  
  def where(criteria:SqlCriteria) = 
    new ViewQuery(store,table,Some(criteria),None)
  
  def iterator = new ViewQuery(store,table,None,None).iterator
}

class ViewQuery(store:XmlStore,table:SqlTable,criteria:Option[SqlCriteria],select:Option[Seq[Symbol]]) extends Iterable[Document]{
  
  def select(symbols:Symbol*) = 
    new ViewQuery(store,table,criteria,Some(symbols))

  def iterator = store.sqlDatabase.readOnly{ implicit connection =>
      
    val tableResults = criteria map {table.findAll.where(_)} getOrElse table.findAll
    
    select match {
      case Some(value) =>
        tableResults.map{
          row => 
          Document(
            store,
            row[String]('_collection),
            row[String]('_id),
            value.map{v=>(v,row[String](v))}:_*
          )
        }.toList.iterator
      case None =>
        tableResults.map{row => Document(store,row[String]('_collection),row[String]('_id))}.toList.iterator
    }
  }
}
