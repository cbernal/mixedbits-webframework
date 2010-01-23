package net.mixedbits.mongo

import net.mixedbits.tools.BlockStatements._

trait MongoWebQuery{
  self:MongoCollection =>
 
  
  def collectParameters(parameters:java.util.Map[String,Array[String]]):String = {
    val consolidatedCriteria = parameters.get(_paramKey)
    if(consolidatedCriteria != null && consolidatedCriteria.length > 0)
      return consolidatedCriteria(0)
    
    val queryStart = _paramKey+":"
    val result = new StringBuffer
    val entries = parameters.entrySet.iterator
    while(entries.hasNext){
      val entry = entries.next
      val key = entry.getKey
      val value = entry.getValue
      if(key startsWith queryStart){
        val parts = key split ":"
        
        if(parts.length == 3 && value.length > 0 && value(0)!=""){
          result.append(parts(1))
          result.append(":")
          result.append(parts(2))
          result.append(":")
          result.append(value(0))
          result.append(";")
        }
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

    /*
  def searchForIds(parameters:java.util.Map[String,Array[String]]) =
    for(constraint <- buildSearchConstraint(parameters))
      yield getIds(constraint)  
  
  def search(parameters:java.util.Map[String,Array[String]]) =
    for(constraint <- buildSearchConstraint(parameters))
      yield find(constraint)
    */
    
  def search(parameters:java.util.Map[String,Array[String]]) = {
    val criteria = collectParameters(parameters)
    buildSearchConstraint(criteria).map{
      constraint =>
      val results = find(constraint)
      (criteria,results,results.size) 
    }
  }
  
  def searchParam(criteriaParam:Option[String],param:String):Option[String] = {
    for(criteria <- criteriaParam)
      yield {
        val result = for(
                  entry <- criteria split ';';
                  if entry startsWith param;
                  parts = entry split ':';
                  if parts.size == 3;
                  if parts(0)+":"+parts(1) == param
                  ) yield parts(2)
        if(result.size == 1)
          result.first
        else
          ""
      }
  }
  
  
  private var _paramKey:String = _
  private var _queryParameters:PartialFunction[String,(String,String)=>MongoConstraint] = _
  
  def queryParameters(paramKey:String)(params:PartialFunction[String,(String,String)=>MongoConstraint]){
    _paramKey = paramKey
    _queryParameters = params 
  }
}
  
