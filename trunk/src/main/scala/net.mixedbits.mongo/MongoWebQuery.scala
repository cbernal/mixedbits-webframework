package net.mixedbits.mongo

import net.mixedbits.tools.BlockStatements._

trait MongoWebQuery{
  self:MongoCollection =>
 
  
  def collectParameters(parameters:java.util.Map[String,Array[String]]):String = {
    val consolidatedCriteria = parameters.get(_paramKey)
    if(consolidatedCriteria != null && consolidatedCriteria.length > 0)
      return consolidatedCriteria(0)
    
    val querySeparator = ":"+_paramKey+":"
    val result = new StringBuffer
    val entries = parameters.entrySet.iterator
    while(entries.hasNext){
      val entry = entries.next
      val parts = entry.getKey split querySeparator
      val value = entry.getValue
      
      if(parts.length == 2 && value.length > 0){
        result.append(parts(0))
        result.append(":")
        result.append(parts(1))
        result.append(":")
        result.append(value(0))
        result.append(";")
      }
    }
    result.toString
  }
  
  def buildSearchConstraint(parameters:java.util.Map[String,Array[String]]):Option[MongoConstraint] = 
    buildSearchConstraint(collectParameters(parameters))
  
  def buildSearchConstraint(criteria:String):Option[MongoConstraint] = {
    attempt{
      val constraints = new MongoConstraintGroup
      
      val entries = criteria split ';'
      for(entry <- entries){
        val parts = entry split ':'
        for( constraint <- extractConstraint( parts(0), parts(1), parts(2) ) )
          constraints += constraint
      }
  
      constraints
    }
  }
  
  def extractConstraint(property:String,operation:String,value:String):Option[MongoConstraint] = {
    return {
            if(_queryParameters isDefinedAt property)
              Some(_queryParameters(property)(operation,value))
            else
              None
            }
  }
    
  def param(property:JsIntProperty) = intParam(property) _
  def param(property:JsDoubleProperty) = doubleParam(property) _
  def param(property:JsStringProperty) = stringParam(property) _

  def intParam(property:JsNumberProperty[Int])(operation:String,value:String):MongoConstraint = 
    numberParam(property,operation,value.toInt)
  def doubleParam(property:JsNumberProperty[Double])(operation:String,value:String):MongoConstraint = 
    numberParam(property,operation,value.toDouble)
  def numberParam[T <: AnyVal](property:JsNumberProperty[T], operation:String, value: => T):MongoConstraint =
    operation match {
      case "eq" => property == value
      case "ne" => property != value
      case "gt" => property > value
      case "gte" => property >= value
      case "lt" => property < value
      case "lte" => property <= value
    }
    
  def stringParam(property:JsProperty[String])(operation:String, value:String):MongoConstraint =
    operation match {
      case "eq" => property == value
      case "ne" => property != value
    }

  def searchForIds(parameters:java.util.Map[String,Array[String]]) =
    for(constraint <- buildSearchConstraint(parameters))
      yield getIds(constraint)  
  
  def search(parameters:java.util.Map[String,Array[String]]) =
    for(constraint <- buildSearchConstraint(parameters))
      yield find(constraint)
  
  
  private var _paramKey:String = _
  private var _queryParameters:PartialFunction[String,(String,String)=>MongoConstraint] = _
  
  def queryParameters(paramKey:String)(params:PartialFunction[String,(String,String)=>MongoConstraint]){
    _paramKey = paramKey
    _queryParameters = params 
  }
}
  