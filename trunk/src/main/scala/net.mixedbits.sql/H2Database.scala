package net.mixedbits.sql

import java.sql._
import javax.sql._

case class H2Database(datasource:DataSource,schemaName:String) extends SqlDatabase{
  def getReadConnection = datasource.getConnection
  def getWriteConnection = datasource.getConnection
  
  def createTable(table:SqlTable)(implicit connection:SqlWriteConnection) = {
    val primaryKey = table.primaryKey map {"`"+_+"`"} mkString ("PRIMARY KEY(",",",")")
    val columnDefinitions = table.columns map sqlColumnToSqlColumnDefinition
    val tableDefinition = (columnDefinitions ++ (if(table.primaryKey.size > 0) Seq(primaryKey) else Nil)) mkString ","
    
    table.execute("CREATE SCHEMA IF NOT EXISTS `%1$s`;".format( schemaName ))
    table.execute("CREATE TABLE IF NOT EXISTS `%1$s`.`%2$s` ( %3$s );".format( schemaName, table.name, tableDefinition))
  }

  override def exists(table:SqlTable)(implicit connection:SqlConnection) = 
    connection.rawConnection.getMetaData().getTables(null,connection.database.schemaName.toUpperCase,table.name.toUpperCase,null).first()
  
  override def existingColumnNames(table:SqlTable)(implicit connection:SqlConnection):Seq[String] = 
    (for(column <- connection.rawConnection.getMetaData().getColumns(null,connection.database.schemaName.toUpperCase,table.name.toUpperCase,null))
      yield column[String]('COLUMN_NAME)).toList

}
