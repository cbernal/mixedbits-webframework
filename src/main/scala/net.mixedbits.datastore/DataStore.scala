package net.mixedbits.datastore

import net.mixedbits.tools._
import net.mixedbits.sql._
import java.sql._

class DataStore(schema:String,documentsTable:String){
  datastore =>
 
  
  val _collection:SqlColumn = '_collection -> SqlChar(128)
  val _id:SqlColumn = '_id -> SqlChar(128)
  val _row:SqlColumn = '_row -> SqlAutoIncrementColumn                  
  val _documents = SqlTable(documentsTable,"_collection","_id")(
                      _collection,
                      _id,
                      '_created -> SqlDateTimeColumn,
                      '_modified -> SqlDateTimeColumn,
                      '_document -> SqlMediumBlob
                    )
                    
                    
  private val views = new scala.collection.mutable.ListBuffer[View]
  private val collections = new scala.collection.mutable.ListBuffer[Collection[_]]
  
  private var validated = false
  private def validate(implicit connection:SqlWriteConnection){
    if(validated)
      return;
    validated = true
    _documents.validateStructure
    views map {_.table} foreach {_.validateStructure}
  }

  
  class Row(val values:Seq[Any])
  case class ViewQuery(view:View,criteria:SqlCriteria)
  class View(val name:String,columns:Seq[SqlColumn]){
    views += this
    val table = SqlTable(name,"_collection","_id","_row")(Seq(_collection,_id,_row) ++ columns:_*)
    def store[T <: DataObject](rowExtractor: T => Seq[Row]):(SqlWriteConnection,Collection[T],T) => Unit = {(connection:SqlWriteConnection,collection:Collection[T],value:T) =>
      implicit val con = connection
    
      val id = collection.idExtractor(value).value
      
      //remove old values
      table.findAll where ('_collection === collection.name and '_id === id) delete
      
      //store new values
      for(newRow <- rowExtractor(value)) table.insert{ row =>
        row('_collection) = collection.name
        row('_id) = id

        for( (newValue,SqlColumn(name,columnType)) <- newRow.values zip columns){
          columnType match {
            case SqlStringColumn(_) => row(Symbol(name)) = newValue.asInstanceOf[String]
            case SqlIntColumn(size) => size match {
              case SqlInt8 => row(Symbol(name)) = newValue.asInstanceOf[Byte]
              case SqlInt16 => row(Symbol(name)) = newValue.asInstanceOf[Short]
              case SqlInt32 => row(Symbol(name)) = newValue.asInstanceOf[Int]
              case SqlInt64 => row(Symbol(name)) = newValue.asInstanceOf[Long]
            }
            case SqlFloatColumn(size) => size match {
              case SqlFloat32 => row(Symbol(name)) = newValue.asInstanceOf[Float]
              case SqlFloat64 => row(Symbol(name)) = newValue.asInstanceOf[Double]
            }
            case SqlBlobColumn(_) => row(Symbol(name)) = newValue.asInstanceOf[Blob] //???
            case SqlDateTimeColumn => row(Symbol(name)) = newValue.asInstanceOf[java.util.Date]
            case SqlBoolColumn => row(Symbol(name)) = newValue.asInstanceOf[Boolean]
            case SqlAutoIncrementColumn => row(Symbol(name)) = newValue.asInstanceOf[Long]
          }
        }
      }
    }
    def matches(criteria:SqlCriteria) = ViewQuery(this,criteria)
  }

  class Collection[T <: DataObject:ClassManifest](val name:String,val idExtractor: T=>T#Property[String],val viewStoreFunctions: Seq[(SqlWriteConnection,Collection[T],T)=>Any]){
    collections += this
  
    def store(value:T)(implicit connection:SqlWriteConnection){
      
      val id = idExtractor(value).value

      _documents.insertOrUpdate(name,id){
        (row,isUpdate) =>
        val now = new java.util.Date
        if(!isUpdate) row('_created) = now
        row('_modified) = now
        
        //val blob = connection.rawConnection.createBlob()
        //blob.setBytes(0,org.bson.BSON.encode(value.convertTo[org.bson.BSONObject]))
        //row('_document) = blob
        row('_document) = org.bson.BSON.encode(value.convertTo[org.bson.BSONObject])
      }
      
      for(function <- viewStoreFunctions)
        function(connection,this,value)
    }
    
    def findAll(viewQueries:ViewQuery*)(implicit connection:SqlConnection):Iterator[T] = new Iterator[T]{
      
      type CapturedValue = (PreparedStatement,Int)=>Unit
      
      def extractWhereClause(criteria:SqlCriteria,table:String):(String,List[CapturedValue]) = {
        val functions = new scala.collection.mutable.ListBuffer[CapturedValue]()
        val clause = criteriaToClause(criteria,table,{functions += _})
        (clause,functions.toList)
      }
      
      def criteriaToClause(criteria:SqlCriteria,table:String,f:CapturedValue=>Unit):String = criteria match {
        case SqlCriteriaGroup(groupType,left,right) =>
          "("+criteriaToClause(left,table,f) + " " + groupType + " " + criteriaToClause(right,table,f)+")"
        case criterion@SqlCriterion(name,operation,_) =>
          f(criterion.capturedValue)
          "`%1$s`.`%2$s` %3$s ?".format(table,name,operation) 
      }
      
      val statement = {
        if(viewQueries.size == 0){
          val s = connection.rawConnection.prepareStatement("SELECT documents._collection as _collection,documents._id as _id, documents._document as _document FROM `%1$s`.`%2$s` as documents WHERE documents._collection = ?".format(schema,documentsTable))
          s.setString(1,name)
          s
        }
        else if(viewQueries.size == 1){
          val query = viewQueries.head
          val (clause,functions) = extractWhereClause(query.criteria,"querytable1")
          val s = connection.rawConnection.prepareStatement("SELECT documents._collection as _collection,documents._id as _id, documents._document as _document FROM `%1$s`.`%2$s` as documents LEFT JOIN  `%1$s`.`%3$s` as querytable1 ON (documents._collection = querytable1._collection AND documents._id = querytable1._id) WHERE documents._collection = ? AND %4$s".format(schema,documentsTable,query.view.name,clause))
          s.setString(1,name)
          for( (function,index) <- functions.zipWithIndex )
            function(s,index+2)
          s
        }
        else{
          error("2 or more queries are not currently supported")
        }
      }
      
      val results = statement.executeQuery()
      connection.runOnClose{ results.close() }
      
      
      
      def hasNext = results.next()
      def next = {
        val blob = results.getBlob("_document")
        val decoder = new org.bson.BSONDecoder()
        DataObject.load[T,org.bson.BSONObject](decoder.readObject(blob.getBinaryStream))
      }
    }
    
  }
  
  def row(values:Any*) = new Row(values)
  def view(name:String,columns:SqlColumn*) = new View(name,columns)
  def collection[T <: DataObject:ClassManifest](name:String)(idExtractor: T=>T#Property[String])(viewStoreFunctions: ((SqlWriteConnection,Collection[T],T)=>Any)*) = new Collection(name,idExtractor,viewStoreFunctions)  
}

case class DataRecord(collection:String,id:String,columns:(Symbol,String)*){
  def apply(symbol:Symbol) = columns.filter(_._1 == symbol).head._2
  def column(symbol:Symbol) = columns.filter(_._1 == symbol).headOption.map(_._2)
}

object DataStore{
  object Listing extends DataCompanion[Listing]
  class Listing extends DataObject{
    object id extends DefaultProperty("")
    object propertyType extends DefaultProperty("")
    object price extends DefaultProperty(0.0f)
    object awesome extends DefaultProperty(false)
    object tags extends SeqProperty[String]
  }
  
  object Documents extends DataStore("test","_json"){
    val realestate = view("realestate",'price -> SqlFloat32,'propertyType -> SqlChar(64),'awesome -> SqlBoolColumn)
    val tags = view("tags",'tag -> SqlChar(64))
    val listings = collection[Listing]("listings"){_.id}(
      tags.store{_.tags map {row(_)}},
      realestate.store{listing => row(listing.price(),listing.propertyType(),listing.awesome()) :: Nil}
    )
    database { implicit connection => validate }
  }
  
  val database = {
    val ds = new org.h2.jdbcx.JdbcDataSource()
    ds.setURL("jdbc:h2:~/bson-data-store")
    ds.setUser("sa")
    ds.setPassword("sa")
    H2Database(ds,"test")
  }
  
  def test(){
    database { implicit connection =>
      println("storing...")
      for(price <- List(1000,100000,1000000); tags <- List(List("teh","awesome"),List("teh"),List("awesome")))
        Documents.listings.store(Listing().id(net.mixedbits.xmlstore.UniqueId()).price(price).tags(tags:_*))

      println("querying...")
      for( listing <- Documents.listings findAll (
                        Documents.realestate matches ('price >= 100000)
                      )) println(listing)

      println("querying...")
      for( listing <- Documents.listings findAll (
                        Documents.tags matches ('tag === "teh")
                      )) println(listing)
                      
      println("iterating...")
      Documents.listings findAll() foreach println
    }
      //println("querying...")
      //for( listing <- Documents.listings findAll (
      //                  Documents.realestate matches ('price >= 100000),
      //                  Documents.tags matches ('tag === "teh")
      //                )) println(listing)
                      


  }
  
}
