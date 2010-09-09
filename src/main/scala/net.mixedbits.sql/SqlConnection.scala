package net.mixedbits.sql
import java.sql._


trait SqlConnection extends java.io.Closeable{
  val rawConnection:Connection
  val database:SqlDatabase
  
  val _runOnClose = new scala.collection.mutable.ListBuffer[()=>Unit]()
  def runOnClose(f: =>Unit):Unit = 
    _runOnClose += {()=>f}
  
  def close() = {
    _runOnClose foreach {f => 
      try{
        f()
      }catch{
        case _ => ()
      }
    }
    rawConnection.close()
  }
}

class SqlReadConnection(val rawConnection:Connection,val database:SqlDatabase) extends SqlConnection
class SqlWriteConnection(val rawConnection:Connection,val database:SqlDatabase) extends SqlConnection
