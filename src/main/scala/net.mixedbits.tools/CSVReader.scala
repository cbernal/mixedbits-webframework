package net.mixedbits.tools

import java.io._
import org.supercsv.io._
import org.supercsv.prefs._
import scala.collection.JavaConversions._

class CSVReader(reader:Reader,headers:Array[String] = null,preferences:CsvPreference = null) extends Iterable[Map[String,String]] with Closeable{
  csv =>
  private lazy val csvTokenizer = new Tokenizer(reader,Option(preferences) getOrElse CsvPreference.EXCEL_PREFERENCE)
  def close() = csvTokenizer.close()
  def readLine() = {
    val line = new java.util.ArrayList[String]
    csvTokenizer.readStringList(line)
    line.toArray(Array.ofDim[String](line.size))
  }
  def iterator = new Iterator[Map[String,String]]{
    val headers = Option(csv.headers) getOrElse readLine()
    var current:Array[String] = null
    def hasNext() = {
      current = readLine()
      current.size match {
        case x if x == headers.size =>
          true
        case _ =>
          close()
          false
      }
    }
    def next() = Map(headers zip current:_*)
  }
}
