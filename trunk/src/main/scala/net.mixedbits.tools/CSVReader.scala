package net.mixedbits.tools

import java.io._
import org.supercsv.io._
import org.supercsv.prefs._
    
class CSVReader(file:File,headers:Array[String] = null) extends Iterable[scala.collection.mutable.Map[String,String]]{
  def headerOption = Option(headers)
          
  def iterator = new Iterator[scala.collection.mutable.Map[String,String]]{
    private lazy val reader = new CsvMapReader(new FileReader(file),CsvPreference.EXCEL_PREFERENCE)
    private lazy val header = headerOption getOrElse reader.getCSVHeader(true)
    
    private var current:scala.collection.mutable.Map[String,String] = null
    
    def next():scala.collection.mutable.Map[String,String] = current
    def hasNext():Boolean = {
      val line = reader.read(header:_*)
      if(line == null){
        reader.close
        false
      }
      else{
        current = scala.collection.JavaConversions.asMap(line)
        true
      }
    }
  }
}
