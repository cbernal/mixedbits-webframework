package net.mixedbits.sql

import java.sql._
import sys.error

case class SqlTable(name:String,primaryKey:String*)(val columns:SqlColumn*){
  lazy val autoIncrementColumnCount = columns.filter{_.columnType == SqlAutoIncrementColumn}.size
  
  type CapturedValue = (PreparedStatement,Int)=>Unit
  
  protected[sql] def extractWhereClause(criteria:SqlCriteria):(String,List[CapturedValue]) = {
    val functions = new scala.collection.mutable.ListBuffer[CapturedValue]()
    val clause = criteriaToClause(criteria,{functions += _})
    (clause,functions.toList)
  }
  
  protected[sql] def criteriaToClause(criteria:SqlCriteria,f:CapturedValue=>Unit):String = criteria match {
    case SqlCriteriaGroup(groupType,left,right) =>
      "("+criteriaToClause(left,f) + " " + groupType + " " + criteriaToClause(right,f)+")"
    case criterion@SqlCriterion(name,operation,_) =>
      f(criterion.capturedValue)
      "`%1$s` %2$s ?".format(name,operation) 
  }
  
  protected[sql] def execute(stmt:String)(implicit connection:SqlConnection) = {
    val statement = connection.rawConnection.createStatement()
    try {
      statement.executeUpdate(stmt)
    } finally {
      statement.close()
    }
  }
  
  protected[sql] def results[T](stmt:String,creationFlags:Int*)(f:ResultSet=>T)(implicit connection:SqlConnection):T = {
    
    val statement = creationFlags match {
      case Seq(resultSetType,resultSetConcurrency,resultSetHoldability) =>
        connection.rawConnection.createStatement(resultSetType,resultSetConcurrency,resultSetHoldability)
      case Seq(resultSetType,resultSetConcurrency) =>
        connection.rawConnection.createStatement(resultSetType,resultSetConcurrency)
      case Seq() =>
        connection.rawConnection.createStatement()
      case _ =>
        error("Only 3 flags are allowed to be set, in the following order: "+
              "resultSetType, resultSetConcurrency, resultSetHoldability, as per documentation on Connection.createStatement")
    }
    
    var resultSet = statement.executeQuery(stmt)

    connection.runOnClose{
      if(resultSet!=null)
        resultSet.close()
      statement.close()
    }
    
    f(resultSet) 
  }
  
  protected[sql] def createPreparedStatement[T](stmt:String,creationFlags:Int*)(f:PreparedStatement=>T)(implicit connection:SqlConnection):T = {
    val statement = creationFlags match {
      case Seq(resultSetType,resultSetConcurrency,resultSetHoldability) =>
        connection.rawConnection.prepareStatement(stmt,resultSetType,resultSetConcurrency,resultSetHoldability)
      case Seq(resultSetType,resultSetConcurrency) =>
        connection.rawConnection.prepareStatement(stmt,resultSetType,resultSetConcurrency)
      case Seq() =>
        connection.rawConnection.prepareStatement(stmt)
      case _ =>
        error("Only 3 flags are allowed to be set, in the following order: "+
              "resultSetType, resultSetConcurrency, resultSetHoldability, as per documentation on Connection.createStatement")
    }

    connection.runOnClose{
      statement.close()
    }
    
    f(statement)
  }
  
  //table structure operations
  def create(implicit connection:SqlWriteConnection) = 
    connection.database.createTable(this)
  
  //creates missing columns / tables, never drops columns, never alters columns
  def validateStructure(implicit connection:SqlWriteConnection):Unit = {
    if(!connection.database.exists(this))
      return create

    val existingColumns = connection.database.existingColumnNames(this) map {_.toLowerCase}
    val requiredColumns = this.columns map {_.name.toLowerCase}
    val missingColumns:Seq[SqlColumn] = (requiredColumns diff existingColumns) map (name => this.columns.filter(_.name equalsIgnoreCase name).head)
    missingColumns foreach {connection.database.addColumn(this,_)}
  }
    
  //row operations
  def insert(f:SqlInsert=>Unit)(implicit connection:SqlWriteConnection) = 
    results("SELECT * FROM `%1$s`.`%2$s` LIMIT 1;".format( connection.database.schemaName, this.name),ResultSet.TYPE_SCROLL_INSENSITIVE,ResultSet.CONCUR_UPDATABLE){
      rs =>
      rs.moveToInsertRow()
      f(new SqlInsert(rs))
      rs.insertRow()
    }
    
  def insertOrUpdate(primaryKey:Any*)(f:(SqlInsert,/*isUpdate*/Boolean)=>Unit)(implicit connection:SqlWriteConnection) = {
    val minKeyCount = this.primaryKey.size - this.autoIncrementColumnCount
    val maxKeyCount = this.primaryKey.size
    if(primaryKey.size < minKeyCount || primaryKey.size > maxKeyCount)
      error("invalid primary key: "+primaryKey)
    
    
    val (whereClause,capturedValues) = 
      if(primaryKey.size == 0){
        ("",Nil)
      } else {
        var criteria:SqlCriteria = SqlEmptyCriteria
        
        for( (value,keyName) <- primaryKey zip this.primaryKey )
          criteria = criteria and Symbol(keyName) === value.toString 
        
        val (clause,values) = extractWhereClause( criteria )
        
        ("WHERE "+clause,values)
      }
    
    createPreparedStatement("SELECT * FROM `%1$s`.`%2$s` %3$s LIMIT 1;".format( connection.database.schemaName, this.name, whereClause),
                              ResultSet.TYPE_SCROLL_INSENSITIVE,ResultSet.CONCUR_UPDATABLE) {
      statement =>
      
      for( (f,index) <- capturedValues.zipWithIndex )
        f(statement,index+1)
      
      val rs = statement.executeQuery()
      val insert = new SqlInsert(rs)
      
      //only an update if we have a fully specified primary key
			if(primaryKey.size == maxKeyCount && rs.first()){
        f(insert,true)
        rs.updateRow()
			} else {
				rs.moveToInsertRow()
        
        for( (value,keyName) <- primaryKey zip this.primaryKey )
          insert(Symbol(keyName)) = value.toString 
        
				f(insert,false)
        rs.insertRow()
      }
      
    }
  }
  
  def findAll(implicit connection:SqlConnection) = 
    new SqlTableResults(this) with SqlTableResultsWithWhere
}

trait SqlTableResultsWithWhere{
  self:SqlTableResults =>
  def where(criteria:SqlCriteria) = 
    new SqlTableResults(self.table,Some(criteria))
}

class SqlTableResults(val table:SqlTable,criteria:Option[SqlCriteria] = None)(implicit val connection:SqlConnection) extends Iterable[SqlResult]{
  type CapturedValue = (PreparedStatement,Int)=>Unit
  
  protected lazy val whereClause:Option[(String,List[CapturedValue])] = 
    criteria map {table.extractWhereClause(_)}
    
  def update(f:SqlUpdate=>Unit)(implicit connection:SqlWriteConnection) = {
    val updates = new SqlUpdate
    f(updates)

    val updateColumns = updates.names map{"`%1$s` = ?".format(_)} mkString ", "

    whereClause match {
      case Some( (clause,capturedValues) ) =>
        table.createPreparedStatement("UPDATE `%1$s`.`%2$s` SET %3$s WHERE %4$s;".format( connection.database.schemaName, table.name, updateColumns, clause)) {
          statement =>
          for( (f,index) <- (updates.capturedValues ++ capturedValues).zipWithIndex )
            f(statement,index+1)
          statement.executeUpdate()
        }
      case None =>        
        table.createPreparedStatement("UPDATE `%1$s`.`%2$s` SET %3$s;".format( connection.database.schemaName, table.name, updateColumns)){
          statement =>
          for( (f,index) <- updates.capturedValues.zipWithIndex )
            f(statement,index+1)
          statement.executeUpdate()
        }
    }
  }
  
  def delete(implicit connection:SqlWriteConnection) = whereClause match {
    case Some( (clause,capturedValues) ) =>
      table.createPreparedStatement("DELETE FROM `%1$s`.`%2$s` WHERE %3$s;".format( connection.database.schemaName, table.name, clause )) {
        statement =>
        for( (f,index) <- capturedValues.zipWithIndex )
          f(statement,index+1)
        statement.executeUpdate()
      }
    case None =>
      table.execute( "DELETE FROM `%1$s`.`%2$s`;".format( connection.database.schemaName, table.name ) )
  }
  
  
  def iterator = 
    whereClause match {
      case Some( (clause,capturedValues) ) =>
        table.createPreparedStatement("SELECT * FROM `%1$s`.`%2$s` WHERE %3$s;".format( connection.database.schemaName, table.name, clause )) {
          statement =>
          for( (f,index) <- capturedValues.zipWithIndex )
            f(statement,index+1)
          
          val resultSet = statement.executeQuery()
          connection.runOnClose{ resultSet.close() }
          resultSetToIterator(resultSet)
        }
      case None =>
        table.results("SELECT * FROM `%1$s`.`%2$s`;".format( connection.database.schemaName, table.name )){resultSetToIterator}
    }  
}
