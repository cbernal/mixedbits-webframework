package net.mixedbits.sql

import java.sql._
import javax.sql._

case class MysqlDatabase(schemaName:String,writeDataSource:DataSource,readDataSource:DataSource = null) extends SqlDatabase{
  private val _readDataSource = Option(readDataSource) getOrElse writeDataSource
  def getReadConnection = _readDataSource.getConnection
  def getWriteConnection = writeDataSource.getConnection
  
  def createTable(table:SqlTable)(implicit connection:SqlWriteConnection) = {
    val primaryKey = table.primaryKey map {"`"+_+"`"} mkString ("PRIMARY KEY(",",",")")
    val columnDefinitions = table.columns map sqlColumnToSqlColumnDefinition
    val tableDefinition = (columnDefinitions ++ (if(table.primaryKey.size > 0) Seq(primaryKey) else Nil)) mkString ","
    
    table.execute("CREATE SCHEMA IF NOT EXISTS `%1$s`;".format( schemaName ))
    table.execute("CREATE TABLE `%1$s`.`%2$s` ( %3$s ) DEFAULT CHARSET=utf8;".format( schemaName, table.name, tableDefinition))
  }

}
