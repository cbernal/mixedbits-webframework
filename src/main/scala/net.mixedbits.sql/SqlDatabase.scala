package net.mixedbits.sql

import java.sql._

trait SqlDatabase{
  val schemaName:String
  protected def getReadConnection:Connection
  protected def getWriteConnection:Connection
  
  def createTable(table:SqlTable)(implicit connection:SqlWriteConnection):Unit
  
  def exists(table:SqlTable)(implicit connection:SqlConnection) =
    connection.rawConnection.getMetaData().getTables(connection.database.schemaName,null,table.name,null).first()
  
  def existingColumnNames(table:SqlTable)(implicit connection:SqlConnection):Seq[String] = 
    (for(column <- connection.rawConnection.getMetaData().getColumns(connection.database.schemaName,null,table.name,null))
      yield column[String]('COLUMN_NAME)).toList
  
  protected def withReadConnection[T](f:SqlConnection=>T):T = {
    val conn = readConnection
    try{
      f(conn)
    } finally {
      conn.close()
    }
  }
  
  protected def withWriteConnection[T](f:SqlWriteConnection=>T):T = {
    val conn = writeConnection
    try{
      f(conn)
    } finally {
      conn.close()
    }
  }

  def addColumn(table:SqlTable,column:SqlColumn)(implicit connection:SqlWriteConnection) = 
    table.execute( "ALTER TABLE `%1$s`.`%2$s` ADD %3$s;".format( schemaName, table.name, sqlColumnToSqlColumnDefinition(column) ) )
  
  protected def sqlColumnToSqlColumnDefinition(column:SqlColumn) = 
    column match {
      case SqlColumn(name,columnType) =>
        val (typeDefinition,otherStuff) = columnType match {
          case SqlStringColumn(size) => size match {
            case SqlChar(value) => ("CHAR("+value+")","")
            case SqlVarChar(value) => ("VARCHAR("+value+")","")
            case SqlSmallText => ("TEXT","")
            case SqlMediumText => ("MEDIUMTEXT","")
            case SqlLargeText => ("LONGTEXT","")
          } 
          case SqlIntColumn(size) => size match {
            case SqlInt8 => ("TINYINT","")
            case SqlInt16 => ("SMALLINT","")
            case SqlInt32 => ("INT","")
            case SqlInt64 => ("LONG","")
          }
          case SqlFloatColumn => ("FLOAT","")
          case SqlDateTimeColumn => ("DATETIME","")
          case SqlBoolColumn => ("BOOL","")
          case SqlAutoIncrementColumn => ("BIGINT","auto_increment")
        }
        
      "`%1$s` %2$s default NULL %3$s".format(name,typeDefinition,otherStuff)
    }
  
  def readOnly[T](f:SqlConnection=>T):T = withReadConnection(f)
  def apply[T](f:SqlWriteConnection=>T):T = withWriteConnection(f)

  def readConnection() = new SqlReadConnection(getReadConnection,this)
  def writeConnection() = new SqlWriteConnection(getWriteConnection,this)
  
}
