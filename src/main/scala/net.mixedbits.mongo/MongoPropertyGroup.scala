package net.mixedbits.mongo

import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._
import com.mongodb._

import scala.collection.mutable.ListBuffer

class JsPropertyGroup{
  
  def this(a:JsProperty[_]) = {
    this()
    this += a
  }
  
  def this(a:JsProperty[_],b:JsProperty[_]) = {
    this()
    this += a
    this += b
  }
  
  protected val _properties = new ListBuffer[JsProperty[_]]
  def properties = _properties.readOnly
  
  def and(property:JsProperty[_]):JsPropertyGroup =
    this += property
  
  def += (property:JsProperty[_]):JsPropertyGroup = {
    _properties += property
    this
  }
}
