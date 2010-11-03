package net.mixedbits.tools

import java.io._
import scala.collection.JavaConversions._

class CSVReader(reader:Reader,headers:Array[String] = null,preferences:CSVPreference = CSVPreference()) extends Iterable[Map[String,String]] with Closeable{
  csv =>
  private lazy val csvReader = new au.com.bytecode.opencsv.CSVReader(reader,preferences.separator,preferences.quotechar,preferences.escape)
  def close() = csvReader.close()
 
  def iterator = new Iterator[Map[String,String]]{
    val headers = Option(csv.headers) getOrElse csvReader.readNext()
    var current:Array[String] = null
    def hasNext():Boolean = {
      current = csvReader.readNext()
      
      if(current == null || current.size != headers.size){
        close()
        return false
      }
      
      return true
    }
    def next() = Map(headers zip current:_*)
  }
}

case class CSVPreference(separator:Char = ',', quotechar:Char = '"', escape:Char = '\\')
