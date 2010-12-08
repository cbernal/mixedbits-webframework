package net.mixedbits.tools

import org.bson._
import org.bson.types.BasicBSONList
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
      case obj:BSONObject => Some(manifest[T].erasure.newInstance.asInstanceOf[T].loadJsonData(obj))
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
      case list:BasicBSONList => Some(new DataSeq(list))
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

class DataSeq[T:DataValue](protected[tools] val jsonList:BasicBSONList) extends scala.collection.mutable.Seq[T]{
  def this() = this(new BasicBSONList)
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
    new DataSeq({val list = new BasicBSONList;seq foreach {x => list.add(implicitly[DataValue[T]].toRawType(x).asInstanceOf[AnyRef])};list})
}

object DataObject{
  def newInstance[T <: DataObject:ClassManifest] = 
    implicitly[ClassManifest[T]].erasure.newInstance().asInstanceOf[T]
  
  def load[T <: DataObject : ClassManifest,F:DataFormat](value:F) = 
    newInstance[T].loadJsonData(implicitly[DataFormat[F]].decode(value))
  
  def fromJson[T <: DataObject:ClassManifest](json:String) =
    newInstance[T].loadJsonData(com.mongodb.util.JSON.parse(json).asInstanceOf[com.mongodb.BasicDBObject])
  
  def fromBson[T <: DataObject:ClassManifest](input:java.io.InputStream) = 
    newInstance[T].loadJsonData(new BSONDecoder().readObject(input))
  
  def fromBson[T <: DataObject:ClassManifest](bytes:Array[Byte]) = 
    newInstance[T].loadJsonData(new BSONDecoder().readObject(bytes))
}

trait DataObject{
  dataObject =>
  private val className = Objects.classNameParts(getClass.getName).last
  def tagName = className
  
  def tag = get[String](jsonData,"#") getOrElse tagName
  
  def convertTo[T:DataFormat] = 
    implicitly[DataFormat[T]].encode(jsonData)
  
  def as[T <: DataObject:ClassManifest] = 
    DataObject.newInstance[T].loadJsonData(jsonData)
  
  def toJson = dataObject.toString
  
  def toBson = BSON.encode(dataObject.jsonData)
  
  protected[tools] def loadJsonData(data:BSONObject):dataObject.type = {jsonData = data;this}
  
  protected[tools] var jsonData:BSONObject = new BasicBSONObject("#",tagName)
  
  private def get[T:DataValue](obj:BSONObject,names:String*):Option[T] = {
    def recurse(obj:Option[BSONObject],names:List[String]):Option[BSONObject] = names match {
      case Nil => obj
      case head :: tail => recurse(obj map {_.get(head)} collect {case o:BSONObject => o},tail)
    }
    recurse(Option(obj),names.dropRight(1).toList) flatMap {x => implicitly[DataValue[T]].toProperType(x.get(names.last))}
  }
  
  private def ensureExists(obj:BSONObject,names:String*):BSONObject = {
    def recurse(obj:BSONObject,names:List[String]):BSONObject = names match{
      case Nil => obj
      case head :: tail => recurse(Option(obj.get(head)) collect {case o:BSONObject => o} getOrElse { new BasicBSONObject |>> {obj.put(head,_)} },tail)
    }
    recurse(obj,names.toList)
  }
  
  private def put(obj:BSONObject,names:String*)(value:Any) = 
    ensureExists(obj,names dropRight 1:_*).put(names.last,value)

  sealed abstract class Property[T:DataValue]{
    property =>
    
    lazy val propertyName:String = Objects.simpleClassName(property)
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
    private def current:DataSeq[T] = {
      if(_current == null)
        current = get[DataSeq[T]](jsonData,propertyPath:_*) getOrElse new DataSeq[T]
      _current
    }
    private def current_=(value:DataSeq[T]) {
      _current = value;
      put(jsonData,propertyPath:_*)(current.jsonList)
    } 
    def apply():DataSeq[T] = current
    def update(value:DataSeq[T]) = {current = value;dataObject}
    def apply(values:T*):dataObject.type = update(DataSeq(values))
  }
  
  override def toString = jsonData.toString
}

class DataCompanion[T <: DataObject:Manifest]{
  def apply():T = manifest[T].erasure.newInstance().asInstanceOf[T]
 
  def load[F:DataFormat](value:F) = DataObject.load[T,F](value)
  def fromJson(json:String) = DataObject.fromJson[T](json)  
  def fromBson[T <: DataObject:ClassManifest](input:java.io.InputStream) = DataObject.fromBson[T](input)
  def fromBson[T <: DataObject:ClassManifest](bytes:Array[Byte]) = DataObject.fromBson[T](bytes)    
}

trait DataFormat[T]{
  def encode(obj:BSONObject):T
  def decode(encoded:T):BSONObject
}

object DataFormat{
  implicit object BSONObjectFormat extends DataFormat[BSONObject]{
    def encode(obj:BSONObject) = obj
    def decode(obj:BSONObject) = obj
  }
  implicit object XmlElemFormat extends DataFormat[scala.xml.Elem]{
    import scala.xml._
    def encode(obj:BSONObject):Elem = copy(obj)
    
    def copy(src:BSONObject,name:Option[String] = None):Elem = (
      <object>
      {for( key <- src.keySet if key != "#"; value = src.get(key) ) yield value match {
        case value:BasicBSONList => createList(key,value)
        case value:BSONObject => copy(value,Some(key))
        case _ => createValue(key,value)
      }}
      </object>
    ).copy(label=Option(src.get("#")) collect {case s:String => s} orElse name getOrElse "object")

    def createValue(name:String,value:Any):Elem = <value>{value.toString}</value>.copy(label=name)
    
    def createList(name:String,values:BasicBSONList):Elem = (
      <list type="seq">
      {for( value <- values) yield value match {
        case value:BSONObject => copy(value)
        case _ => createValue("value",value)
      }}
      </list>
    ).copy(label=name)
    
    def decode(encoded:Elem):BSONObject = 
      decodeObject(encoded,true)
    
    def decodeObject(encoded:Elem,root:Boolean = false):BSONObject = {
      val obj = if(root) new BasicBSONObject("#",encoded.label) else new BasicBSONObject
      for(element <- encoded.child collect {case e:Elem => e}) element match {
        case Value(text) => obj.put(element.label,text)
        case _ if (element \ "@type" text) == "seq" => obj.put(element.label,decodeList(element))
        case _ => obj.put(element.label,decodeObject(element))
      }
      obj
    }
    
    def decodeList(encoded:Elem):BasicBSONList = {
      val list = new BasicBSONList
      for(element <- encoded.child collect {case e:Elem => e}) element match {
        case Value(text) => list.add(text)
        case _ => list.add(decodeObject(element,true))
      }
      list
    }
    
    object Value{
      def unapply(e:Elem) = 
        if(e.child.size == 1)
          e.child collect {case t:Atom[_] => t.text} headOption
        else
          None
    }
    
  }
  implicit object XmlDomFormat extends DataFormat[org.w3c.dom.Element]{
    import org.w3c.dom._
    def encode(obj:BSONObject):Element = {
      implicit val doc = Xml.createDocument
      doc.appendChild(copy(obj))
      doc.getDocumentElement
    }
    
    def copy(src:BSONObject,name:Option[String] = None)( implicit doc:Document):Element = {
      val node = doc.createElement(Option(src.get("#")) collect {case s:String => s} orElse name getOrElse "object")
      
      for( key <- src.keySet if key != "#"; value = src.get(key)) value match {
        case value:BasicBSONList => node.appendChild(createList(key,value))
        case value:BSONObject => node.appendChild(copy(value,Some(key)))
        case _ => node.appendChild(createValue(key,value))
      }
      
      node
    }
    
    def createValue(name:String,value:Any)(implicit doc:Document):Node = {
      val node = doc.createElement(name)
      node.appendChild(doc.createTextNode(value.toString))
      node
    }
    
    def createList(name:String,values:BasicBSONList)(implicit doc:Document):Node = {
      val node = doc.createElement(name)
      node.setAttribute("type","seq")
      for( value <- values) value match {
        case value:BSONObject => node.appendChild(copy(value))
        case _ => node.appendChild(createValue("value",value))
      }
      node
    }
    
    
    def decode(encoded:Element):BSONObject = 
      decodeObject(encoded,true)
    
    def decodeObject(encoded:Element,root:Boolean = false):BSONObject = {
      val obj = if(root) new BasicBSONObject("#",encoded.getTagName) else new BasicBSONObject
      for(element <- Xml.elements(encoded,"*")) element match {
        case Value(text) => obj.put(element.getTagName,text)
        case _ if element.getAttribute("type") == "seq" => obj.put(element.getTagName,decodeList(element))
        case _ => obj.put(element.getTagName,decodeObject(element))
      }
      obj
    }
    
    def decodeList(encoded:Element):BasicBSONList = {
      val list = new BasicBSONList
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


