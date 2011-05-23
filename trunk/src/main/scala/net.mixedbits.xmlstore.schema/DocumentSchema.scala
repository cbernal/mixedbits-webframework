package net.mixedbits.xmlstore.schema

import scala.xml._
import sys.error

case class DocumentSchema(name:String,version:String,views:Seq[DocumentView],collections:Seq[DocumentCollection])

object DocumentSchema{
  
  def exampleSchema =
    <Schema name="App" version="1.0">
      <View name="Credentials" select="/Credentials">
        <Column type="string" name="username" select="@username" />
        <Column type="string" name="password" select="@password" />
      </View>
      
      <Collection name="Admin" documentId="/User/@id">
        <View ref="Credentials" select="/Profile/Credentials">
          <Column ref="username" select="@email" />
        </View>
      </Collection>
      
      <Collection name="User" documentId="/User/@id">
        <View ref="Credentials" />
        <View name="Contacts" select="//Contact">
          <Column type="string" name="name" select="@name" />
        </View>
      </Collection>
    </Schema>
        
  def test = DocumentSchema(exampleSchema)
  
  def apply(elem:Elem):DocumentSchema = 
    apply(elem.toString)
  
  def apply(schemaXml:String):DocumentSchema = {
    val doc = XML.loadString(schemaXml)
    
    //validate that view and collection names are all distinct
    val viewNames = (doc \\ "View" \ "@name") map {_.text} filter {_!=""}
    val collectionNames = (doc \ "Collection" \ "@name") map {_.text} filter {_!=""}
    
    if(viewNames.distinct.size != viewNames.size)
      error("Duplicate view names were discovered in the schema")
    
    if(collectionNames.distinct.size != collectionNames.size)
      error("Duplicate collection names were discovered in the schema")
    
    //start loading the schema
    val name = doc \ "@name" text
    val version = doc \ "@version" text 
    val rootViews = doc \ "View" map {parseView(_)}
    val collections = doc \ "Collection" map {
      node =>
      DocumentCollection(node \ "@name" text,node \ "@documentId" text,node \ "View" map {parseView(_,rootViews)})
    }
    val collectionViews = collections.flatMap(_.views)
    
    DocumentSchema(name,version,rootViews ++ (collectionViews filter {view => !rootViews.exists(_.name == view.name)}),collections)
  }
  
  def parseView(viewElement:Node,potentialParents:Seq[DocumentView] = Nil):DocumentView = {
    
    if(viewElement attribute "ref" isDefined){
      val ref = viewElement \ "@ref" text
      val newSelect = Option(viewElement \ "@select" text) filter {_!=""} 
      val parentView = potentialParents.filter(_.name == ref).head
      val columnElements = viewElement \ "Column"

      val newColumns = parentView.columns map {
        column =>
        val redefinedColumnElem = columnElements filter {elem => (elem \ "@ref" text) == column.name} headOption
        
        if(redefinedColumnElem.isDefined)
          parseInheritedColumn(redefinedColumnElem.get,column)
        else
          column
      }
      
      DocumentView(ref,newSelect getOrElse parentView.select, newColumns)
    } else {
      val name = viewElement \ "@name" text
      val select = viewElement \ "@select" text
      val columns = viewElement \ "Column" map parseColumn
      
      DocumentView(name,select,columns)
    }
  }
  
  def parseColumn(columnElement:Node):DocumentColumn = 
    DocumentColumn(
      columnElement \ "@name" text,
      columnElement \ "@type" text,
      columnElement \ "@size" text,
      columnElement \ "@select" text
      )
  
  def parseInheritedColumn(columnElement:Node,parent:DocumentColumn):DocumentColumn = 
    DocumentColumn(
      parent.name,
      parent.columnType,
      parent.size,
      Option(columnElement \ "@select" text) filter {_!=""} getOrElse parent.select
      )
}
