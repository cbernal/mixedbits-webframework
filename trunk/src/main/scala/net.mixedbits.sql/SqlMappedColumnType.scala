package net.mixedbits.sql

import java.sql.{Array=>_,_}

sealed abstract class SqlMappedColumnType[T](r:(ResultSet,String) => T)(w:(ResultSet,String,T) => Unit)(u:(PreparedStatement,Int,T) => Unit){
  def read(resultSet:ResultSet,name:String) = r(resultSet,name)
  def write(resultSet:ResultSet,name:String,value:T) = w(resultSet,name,value)
  def update(statement:PreparedStatement,index:Int,value:T) = u(statement,index,value)
  
}

object SqlMappedColumnType{

  implicit object SqlBoolean extends SqlMappedColumnType( _ getBoolean _ )( _ updateBoolean (_,_) )( _ setBoolean (_,_) )
  implicit object SqlByte extends SqlMappedColumnType( _ getByte _ )( _ updateByte (_,_) )( _ setByte (_,_) )
  implicit object SqlShort extends SqlMappedColumnType( _ getShort _ )( _ updateShort (_,_) )( _ setShort (_,_) )
  implicit object SqlInt extends SqlMappedColumnType( _ getInt _ )( _ updateInt (_,_) )( _ setInt (_,_) )
  implicit object SqlLong extends SqlMappedColumnType( _ getLong _ )( _ updateLong (_,_) )( _ setLong (_,_) )
  implicit object SqlFloat extends SqlMappedColumnType( _ getFloat _ )( _ updateFloat (_,_) )( _ setFloat (_,_) )
  implicit object SqlDouble extends SqlMappedColumnType( _ getDouble _ )( _ updateDouble (_,_) )( _ setDouble (_,_) )
  implicit object SqlString extends SqlMappedColumnType( _ getString _ )( _ updateString (_,_) )( _ setString (_,_) )
  implicit object SqlDate extends SqlMappedColumnType[java.util.Date]( _ getTimestamp _ )( _ updateTimestamp (_,_) )( _ setTimestamp (_,_) )
  implicit object SqlBlob extends SqlMappedColumnType[Array[Byte]]( _ getBytes _ )( _ updateBytes (_,_) )( _ setBytes (_,_) )
  
  object SqlObject extends SqlMappedColumnType[Any]( _ getObject _)( _ updateObject (_,_) )( _ setObject (_,_) )
  
}
