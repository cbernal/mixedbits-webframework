package net.mixedbits.tools

import org.json._
import org.jabsorb._
import org.jabsorb.serializer._

class JabsorbDataObjectSerializer extends Serializer{
  private var serializer:JSONSerializer = null
  private val JAVA_CLASS_FIELD = "javaClass"
  def setOwner(_serializer:JSONSerializer){ serializer = _serializer }
  def canSerialize(clazz:Class[_],jsonClazz:Class[_]) = classOf[DataObject].isAssignableFrom(clazz)
  def getSerializableClasses():Array[Class[_]] = Array(classOf[DataObject])
  def getJSONClasses():Array[Class[_]] = Array(classOf[JSONObject])
  def tryUnmarshall(state:SerializerState, clazz:Class[_],o:AnyRef):ObjectMatch = {
    val json = o.asInstanceOf[JSONObject]
    val javaClassName = json.getString(JAVA_CLASS_FIELD)
    val javaClass = Class.forName(javaClassName)
    if(classOf[DataObject].isAssignableFrom(javaClass))
      ObjectMatch.OKAY
    else
      throw new UnmarshallException("not a DataObject...")
  }
  def unmarshall(state:SerializerState, clazz:Class[_], o:AnyRef):AnyRef = {
    val json = o.asInstanceOf[JSONObject]
    val javaClassName = json.getString(JAVA_CLASS_FIELD)
    val javaClass = Class.forName(javaClassName)
    if(classOf[DataObject].isAssignableFrom(javaClass)){
      val bson = implicitly[DataFormat[JSONObject]].decode(json)
      bson.removeField(JAVA_CLASS_FIELD)
      return javaClass.newInstance().asInstanceOf[DataObject].loadJsonData(bson)
    } else {
      throw new UnmarshallException("not a DataObject...")
    }
  }
  def marshall(state:SerializerState, p:AnyRef, o:AnyRef):AnyRef = 
    if(o.isInstanceOf[DataObject])
      return o.asInstanceOf[DataObject].convertTo[JSONObject].put(JAVA_CLASS_FIELD,o.getClass.getName)
    else
      throw new MarshallException("not a DataObject")

}
