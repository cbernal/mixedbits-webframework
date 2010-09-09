package net.mixedbits

import java.sql._

package object sql{
  implicit def symbolToSqlCriterionBuilder(symbol:Symbol) = SqlCriterionBuilder(symbol.name)
  implicit def resultSetToIterator(resultSet:ResultSet) = new Iterator[SqlResult]{
    private val result = new SqlResult(resultSet)
    def hasNext = resultSet.next
    def next = result
  }
  
  implicit def utilDateToTimestamp(date:java.util.Date):java.sql.Timestamp = new java.sql.Timestamp(date.getTime)
  implicit def timestampToUtilDate(date:java.sql.Timestamp):java.util.Date = new java.util.Date(date.getTime)
}
