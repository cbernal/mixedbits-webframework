package net.mixedbits.tools

import com.mongodb._
import scala.collection.JavaConversions._

/*
Data Structure:
  Data Types: object,array,value, the root must always be an object
    Value: something that can be converted to and from a string, like a string, or an int, or a bool or a double :D
    Array: a homogeneous sequence of objects or values
    Object: named sequence of key-value pairs
      Examples:
        {tour:{title:"awesome",residential:{bedrooms:"2",bathrooms:"1"}},tags:["a","b"],photos:[{photo:{comments:"bla"}}]}
        
        Objects: are json objects and potentially contain a '#' label when they are not direct children of another object,
                  otherwise the label is determined by the parents label for the object
        Arrays: json arrays :D
        Values: json values :D
                  
        {"#":"tour",title:"awesome",residential:{bedrooms:"2",bathrooms:"1"},tags:["a","b"],photos:[{"#":"photo",comments:"bla"}]}
        
        Objects: Contain any number of non-text nodes, and no array attribute
        Arrays: Contain any number of the same type of nodes, and an array attribute
        Values: Contain a single text node
        <tour>
          <title>awesome</title>
          <residential>
            <bedrooms>2</bedrooms>
            <bathrooms>1</bathrooms>
          </residential>
          <tags type="seq">
            <value>a<value>
            <value>b</value>
          </tags>
          <photos type="seq">
            <photo>
              <comments>bla</comments>
            </photo>
          </photos>
        </tour>
*/

sealed abstract class DataValue[T]{
  def toProperType(value:Any):Option[T]
  def toRawType(value:T):Any
}

sealed abstract class PrimitiveDataValue[T] extends DataValue[T]{
  def toRawType(value:T) = value
}
object DataValue{
  implicit def objectData[T <: DataObject : Manifest] = new DataValue[T]{
    def toProperType(value:Any) = value match {
      case obj:DBObject => Some(manifest[T].erasure.newInstance.asInstanceOf[T].loadJsonData(obj))
      case _ => None
    }
    def toRawType(value:T) = value.jsonData
  }
  implicit def optionData[T:DataValue] = new DataValue[Option[T]]{
    def toProperType(value:Any) = implicitly[DataValue[T]].toProperType(value) map {Some(_)}
    def toRawType(value:Option[T]) = value map {implicitly[DataValue[T]].toRawType} getOrElse null
  }
  implicit def seqData[T:DataValue] = new DataValue[DataSeq[T]]{
    def toProperType(value:Any) = value match {
      case list:BasicDBList => Some(new DataSeq(list))
      case _ => None
    }
    def toRawType(value:DataSeq[T]) = value.jsonList
  }
  
  implicit object StringData extends PrimitiveDataValue[String]{
    def toProperType(value:Any) = Option(value) map {_.toString}
  }
  
  implicit object BooleanData extends PrimitiveDataValue[Boolean]{
    def toProperType(value:Any) = value match {
      case b:Boolean => Some(b)
      case i:Int => i match {
        case 0 => Some(false)
        case 1 => Some(true)
        case _ => None
      }
      case s:String => s.toLowerCase match {
        case "1"|"yes"|"true" => Some(true)
        case "0"|"no"|"false" => Some(false)
        case _ => None
      }
      case _ => None
    }
  }
  
  implicit object IntData extends PrimitiveDataValue[Int]{
    def toProperType(value:Any) = value match {
      case i:Int => Some(i)
      case l:Long => Some(l.toInt)
      case f:Float => Some(f.toInt)
      case d:Double => Some(d.toInt)
      case s:String => s.parseInt
      case _ => None
    }
  }
  implicit object LongData extends PrimitiveDataValue[Long]{
    def toProperType(value:Any) = value match {
      case i:Int => Some(i.toLong)
      case l:Long => Some(l)
      case f:Float => Some(f.toLong)
      case d:Double => Some(d.toLong)
      case s:String => s.parseLong
      case _ => None
    }
  }
  implicit object FloatData extends PrimitiveDataValue[Float]{
    def toProperType(value:Any) = value match {
      case i:Int => Some(i.toFloat)
      case l:Long => Some(l.toFloat)
      case f:Float => Some(f)
      case d:Double => Some(d.toFloat)
      case s:String => s.parseFloat
      case _ => None
    }
  }
  implicit object DoubleData extends PrimitiveDataValue[Double]{
    def toProperType(value:Any) = value match {
      case i:Int => Some(i.toDouble)
      case l:Long => Some(l.toDouble)
      case f:Float => Some(f.toDouble)
      case d:Double => Some(d)
      case s:String => s.parseDouble
      case _ => None
    }
  }

}

class DataSeq[T:DataValue](protected[tools] val jsonList:BasicDBList) extends scala.collection.mutable.Seq[T]{
  def this() = this(new BasicDBList)
  def update(idx:Int,elem:T) = jsonList.put(idx,implicitly[DataValue[T]].toRawType(elem))
  def apply(idx:Int) = implicitly[DataValue[T]].toProperType(jsonList.get(idx)) getOrElse error("bad data conversion")
  def length = jsonList.size
  def iterator = new Iterator[T]{
    val it = jsonList.iterator
    def hasNext = it.hasNext
    def next = implicitly[DataValue[T]].toProperType(it.next) getOrElse error("bad data conversion")
  }
  
  def += (elem:T){jsonList.add(implicitly[DataValue[T]].toRawType(elem).asInstanceOf[AnyRef])}
}

object DataSeq{
  implicit def apply[T:DataValue](seq:Seq[T]):DataSeq[T] =
    new DataSeq({val list = new BasicDBList;seq foreach {x => list.add(implicitly[DataValue[T]].toRawType(x).asInstanceOf[AnyRef])};list})
}

trait DataObject{
  dataObject =>
  def tagName = getClass.getSimpleName
  
  def convertTo[T:DataFormat] = 
    implicitly[DataFormat[T]].encode(jsonData)
  
  
  protected[tools] def loadJsonData(data:DBObject):dataObject.type = {jsonData = data;this}
  
  protected[tools] var jsonData:DBObject = new BasicDBObject("#",tagName)
  
  private def get[T:DataValue](obj:DBObject,names:String*):Option[T] = {
    def recurse(obj:Option[DBObject],names:List[String]):Option[DBObject] = names match {
      case Nil => obj
      case head :: tail => recurse(obj map {_.get(head)} collect {case o:DBObject => o},tail)
    }
    recurse(Option(obj),names.dropRight(1).toList) flatMap {x => implicitly[DataValue[T]].toProperType(x.get(names.last))}
  }
  
  private def ensureExists(obj:DBObject,names:String*):DBObject = {
    def recurse(obj:DBObject,names:List[String]):DBObject = names match{
      case Nil => obj
      case head :: tail => recurse(Option(obj.get(head)) collect {case o:DBObject => o} getOrElse { new BasicDBObject |>> {obj.put(head,_)} },tail)
    }
    recurse(obj,names.toList)
  }
  
  private def put(obj:DBObject,names:String*)(value:Any) = 
    ensureExists(obj,names dropRight 1:_*).put(names.last,value)

  sealed abstract class Property[T:DataValue]{
    property =>
    
    lazy val propertyName:String = Objects.simpleClassName(this)
    lazy val propertyPath = propertyName.split('.').drop(1)
    def parentPath:Array[String] = propertyPath.take(propertyPath.length-1)
    def shortName:String = propertyPath.last
    
    def exists:Boolean = get(jsonData,propertyPath:_*).isDefined
    def apply(value:T):dataObject.type = update(value)
    
    def value = apply()
    def value_=(newValue:T) = update(newValue)
    
    def update(value:T):dataObject.type
    def apply():T
    
    override def toString() = value.toString
  }
  object Property{
    implicit def propertyToPropertyValue[T](property:Property[T]) = property.value
  }
  class DefaultProperty[T:DataValue](defaultValue:T) extends Property[T]{
    def apply():T = get[T](jsonData,propertyPath:_*) getOrElse defaultValue
    def update(value:T) = {put(jsonData,propertyPath:_*)(value);dataObject}
  }
  class OptionalProperty[T:DataValue] extends Property[Option[T]]{
    def apply():Option[T] = get[T](jsonData,propertyPath:_*)
    def update(value:Option[T]) = {put(jsonData,propertyPath:_*)(value getOrElse null);dataObject}
  }
  class SeqProperty[T:DataValue] extends Property[DataSeq[T]]{
    private var _current:DataSeq[T] = null
    private def current:DataSeq[T] = {if(_current == null){current = new DataSeq[T]};_current}
    private def current_=(value:DataSeq[T]) {_current = value;put(jsonData,propertyPath:_*)(current.jsonList)} 
    def apply():DataSeq[T] = current
    def update(value:DataSeq[T]) = {current = value;dataObject}
    def apply(values:T*):dataObject.type = update(DataSeq(values))
  }
  
  override def toString = jsonData.toString
}

class DataCompanion[T <: DataObject:Manifest]{
  def apply():T = manifest[T].erasure.newInstance().asInstanceOf[T]
  def load[T:DataFormat](value:T) = apply().loadJsonData(implicitly[DataFormat[T]].decode(value))
}

trait DataFormat[T]{
  def encode(obj:DBObject):T
  def decode(encoded:T):DBObject
}

object DataFormat{
  implicit object XmlDomFormat extends DataFormat[org.w3c.dom.Element]{
    import org.w3c.dom._
    def encode(obj:DBObject):Element = {
      implicit val doc = Xml.createDocument
      doc.appendChild(copy(obj))
      doc.getDocumentElement
    }
    
    def copy(src:DBObject,name:Option[String] = None)( implicit doc:Document):Element = {
      val node = doc.createElement(Option(src.get("#")) collect {case s:String => s} orElse name getOrElse "object")
      
      for( key <- src.keySet if key != "#"){
        val value = src.get(key)
        value match {
          case value:BasicDBList => node.appendChild(createList(key,value))
          case value:DBObject => node.appendChild(copy(value,Some(key)))
          case _ => node.appendChild(createValue(key,value))
        }
      }
      
      node
    }
    
    def createValue(name:String,value:Any)(implicit doc:Document):Node = {
      val node = doc.createElement(name)
      node.appendChild(doc.createTextNode(value.toString))
      node
    }
    
    def createList(name:String,values:BasicDBList)(implicit doc:Document):Node = {
      val node = doc.createElement(name)
      node.setAttribute("type","seq")
      for( value <- values) value match {
        case value:DBObject => node.appendChild(copy(value))
        case _ => node.appendChild(createValue("value",value))
      }

      node
    }
    
    
    def decode(encoded:Element):DBObject = 
      decodeObject(encoded,true)
    
    def decodeObject(encoded:Element,root:Boolean = false):DBObject = {
      val obj = if(root) new BasicDBObject("#",encoded.getTagName) else new BasicDBObject
      for(element <- Xml.elements(encoded,"*")) element match {
        case Value(text) => obj.put(element.getTagName,text)
        case _ if element.getAttribute("type") == "seq" => obj.put(element.getTagName,decodeList(element))
        case _ => obj.put(element.getTagName,decodeObject(element))
      }
          
      obj
    }
    
    def decodeList(encoded:Element):BasicDBList = {
      val list = new BasicDBList
      for(element <- Xml.elements(encoded,"*")) element match {
        case Value(text) => list.add(text)
        case _ => list.add(decodeObject(element,true))
      }
      list
    }
    
    object Value{
      def unapply(e:Element) = e.getFirstChild match {
        case text:Text if e.getChildNodes.getLength == 1 =>
          Some(text.getWholeText)
        case _ =>
          None
      }
    }
  }
}


