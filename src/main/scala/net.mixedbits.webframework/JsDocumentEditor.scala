package net.mixedbits.webframework

import net.mixedbits.webframework._
import net.mixedbits.webframework.Tools._
import net.mixedbits.json._

/*
Order:
  Load
  if(isHttpPost)  
    Modify
    Validate
    Save
  Read Values
  Render
*/

trait JsDocumentEditor{
  self: WebPage with WebRequest =>
  
  def load():JsDocument
  def validate():Unit
  def save():Unit
  
  lazy val document:JsDocument = load()
  
  sealed abstract class SaveResult
  case object NotSaved extends SaveResult
  case object SaveSuccess extends SaveResult
  case class SaveError(messages:Seq[String]) extends SaveResult
  
  sealed abstract class ValidateResult
  case object NotValidated extends ValidateResult
  case object ValidateSuccess extends ValidateResult
  case class ValidateError(messages:Seq[String]) extends ValidateResult
  
  private var _saveResult:SaveResult = NotSaved
  private var _validateResult:ValidateResult = NotValidated
  
  protected def saveResult = _saveResult
  protected def validateResult = _validateResult
  
  protected def saveError(messages:Seq[String]){ _saveResult = SaveError(messages) }
  protected def validateError(messages:Seq[String]){ _validateResult = ValidateError(messages) }
  
  def conditions(messages:Option[String]*){
    val list:Seq[String] = for(item <- messages; message <- item) yield message
    if(list.size > 0)
      validateError(list)
  }
  
  def failIf(failCondition:Boolean)(errorMessage:String):Option[String] = if(failCondition) Some(errorMessage) else None
  
  def attemptTo(f: => Any)(errorMessage:String):Option[String] = {
    try{ f; None }
    catch{ case e => Some(errorMessage+" - "+e.getMessage) }
  }
  
    
  def validateResults(body:PartialFunction[ValidateResult,Elements]):Elements = 
    if(body.isDefinedAt(validateResult)) body(validateResult) else Elements()
    
  def saveResults(body:PartialFunction[SaveResult,Elements]):Elements = 
    if(body.isDefinedAt(saveResult)) body(saveResult) else Elements()
  
  trait Property{
    def readValue():String
    def applyUpdates():Unit
    
    def resolve(update:Boolean){
      _oldValue = readValue()
      
      if(update)
        applyUpdates()
      
      _value = readValue()
    }
    
    
    private var _oldValue:String = null
    def oldValue = _oldValue
    
    def hasChanged = _oldValue != _value
    
    private var _value:String = null    
    def apply():String = _value
    override def toString() = _value
  }
  
  private var properties = new scala.collection.mutable.ArrayBuffer[Property]

  def property[T](property:JsProperty[T],paramName:String) = new Property(){
    
    properties += this
    
    def applyUpdates(){
      document(property) = param(paramName).flatMap(property.fromString)
    }
    
    def readValue() = {      
      document(property).map(_.toString) getOrElse ""
    }
  }
  
  
  onBefore{
    
    //only update the document if it is a post
    if(isHttpPost){
      
      //read and update the values
      properties foreach {_.resolve(true)}
      
      //validate the doc
      validate()
      if(_validateResult == NotValidated)
        _validateResult = ValidateSuccess
      
      //save the doc if the validate succeeded
      if(_validateResult == ValidateSuccess){
        save()
        if(_saveResult == NotSaved)
          _saveResult = SaveSuccess
      }
      
    }
    else{    
      //read the values
      properties foreach {_.resolve(false)}
    }
    
  }
  
}
