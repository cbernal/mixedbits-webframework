package net.mixedbits.sql

import java.sql._

class SqlResult(resultSet:ResultSet){
  def apply[T:SqlMappedColumnType](column:Symbol):T = 
    implicitly[SqlMappedColumnType[T]].read(resultSet,column.name)
  
  override def toString() = {
    val metadata = resultSet.getMetaData()
    (
    for(i <- 1 to metadata.getColumnCount)
      yield (metadata.getColumnName(i),resultSet.getObject(i))
    ) mkString ("SqlResult(",", ",")")
  }
    
}

class SqlInsert(resultSet:ResultSet){
  def update[T:SqlMappedColumnType](column:Symbol,value:T) = 
    implicitly[SqlMappedColumnType[T]].write(resultSet,column.name,value)
}

class SqlUpdate(){

  def names = _values map {_._1} toList
  def capturedValues = _values map {_._2} toList
  
  private var _values = new scala.collection.mutable.ListBuffer[(String,(PreparedStatement,Int) => Unit)]()
  
  def update[T:SqlMappedColumnType](column:Symbol,value:T) = 
    _values += (column.name,implicitly[SqlMappedColumnType[T]].update(_,_,value))

}
