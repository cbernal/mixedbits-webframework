package net.mixedbits.sql

import java.sql._

trait SqlCriteria{
  def and(query:SqlCriteria):SqlCriteria = SqlCriteriaGroup("AND",this,query)
  def or(query:SqlCriteria):SqlCriteria = SqlCriteriaGroup("OR",this,query)
}

case class SqlCriteriaGroup(groupType:String,left:SqlCriteria,right:SqlCriteria) extends SqlCriteria
case class SqlCriterion[T:SqlMappedColumnType](name:String,operation:String,value:T) extends SqlCriteria{
  lazy val capturedValue:(PreparedStatement,Int) => Unit = 
    implicitly[SqlMappedColumnType[T]].update(_,_,value)
}

case class SqlCriterionBuilder(name:String){
  def unsafeEquals(value:Any):SqlCriteria = SqlCriterion(name,"=",value)(SqlMappedColumnType.SqlObject)
  def ===[T:SqlMappedColumnType](value:T):SqlCriteria = SqlCriterion(name,"=",value)
  def !==[T:SqlMappedColumnType](value:T):SqlCriteria = SqlCriterion(name,"!=",value)
  def >[T:SqlMappedColumnType](value:T):SqlCriteria = SqlCriterion(name,">",value)
  def >=[T:SqlMappedColumnType](value:T):SqlCriteria = SqlCriterion(name,">=",value)
  def <[T:SqlMappedColumnType](value:T):SqlCriteria = SqlCriterion(name,"<",value)
  def <=[T:SqlMappedColumnType](value:T):SqlCriteria = SqlCriterion(name,"<=",value)
}

object SqlEmptyCriteria extends SqlCriteria{
  override def and(query:SqlCriteria):SqlCriteria = query
  override def or(query:SqlCriteria):SqlCriteria = query
}
