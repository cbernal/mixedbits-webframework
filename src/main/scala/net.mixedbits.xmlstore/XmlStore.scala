package net.mixedbits.xmlstore

import java.sql._
import javax.sql._

import net.mixedbits.xmlstore.schema._
import net.mixedbits.sql._

class XmlStore(val schema:DocumentSchema, val sqlDatabase:SqlDatabase){
  implicit def self = this
  
  val _collections = new scala.collection.mutable.HashMap[String,Collection[AnyRef]]()
  
  def register(collection:Collection[AnyRef]) = 
    _collections += collection.name -> collection
    
  
  def collections(name:String):Collection[AnyRef] = 
    _collections(name)
  
  
  def collection[T <: AnyRef:Manifest](name:String) = new Collection[T](name)
  
  def view(name:String) = views.filter(_.name == name).head
  
  val _collection = SqlColumn("_collection",SqlStringColumn(SqlChar(128)))
  val _id = SqlColumn("_id",SqlStringColumn(SqlChar(128)))
  val _row = SqlColumn("_row",SqlAutoIncrementColumn)
                    
  val _documents = SqlTable("_documents","_collection","_id")(
                      _collection,
                      _id,
                      SqlColumn("created",SqlDateTimeColumn),
                      SqlColumn("modified",SqlDateTimeColumn),
                      SqlColumn("document",SqlStringColumn(SqlMediumText))
                    )

  val viewTables = schema.views map {
    view => SqlTable(view.name,"_collection","_id","_row")(Seq(_collection,_id,_row) ++ view.columns.map(documentColumnToSqlColumn):_*)
  }         
  val views = schema.views.map { view => new View(this,view,viewTables.filter(_.name == view.name).head) }
  
  sqlDatabase{
    implicit connection =>
    _documents.validateStructure
    viewTables foreach {_.validateStructure}
  }
  
  def retrieve(collection:String,id:String) = Document(this,collection,id)
  
  private def constrain(value:Int,min:Int,max:Int) = 
    if(value > max || value < min) {
      println("char size must be between "+min+" and "+max+", '"+value+"' is not valid. defaulting to "+max)
      max
    } else {
      value
    }
  
  def documentColumnToSqlColumn(column:DocumentColumn):SqlColumn = 
    SqlColumn(
      column.name,
      column.columnType match {
        case "string" =>
          SqlStringColumn(column.size match {
            case x if x startsWith "varchar:" =>
              SqlVarChar(constrain((x drop 8).toInt,1,255))
            case x if x startsWith "char:" =>
              SqlChar(constrain((x drop 5).toInt,1,255))
            case "small" =>
              SqlSmallText
            case "medium" =>
              SqlMediumText
            case "large" =>
              SqlLargeText
            case "" =>
              SqlSmallText
            case other =>
              println("found unsupported string size: "+other+", defaulting to small")
              SqlSmallText
          })
        case "int" => 
          SqlIntColumn(column.size match {
            case "8bit" =>
              SqlInt8
            case "16bit" =>
              SqlInt16
            case "32bit" =>
              SqlInt32
            case "64bit" =>
              SqlInt64
            case other =>
              println("found unsupported int size: "+other+", defaulting to 32bit")
              SqlInt32
          })
        case "float" => SqlFloatColumn
        case "datetime" => SqlDateTimeColumn
        case "bool" => SqlBoolColumn
      }
    )  
}
