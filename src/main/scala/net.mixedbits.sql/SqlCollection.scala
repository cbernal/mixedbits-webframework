package net.mixedbits.sql

import java.lang.reflect.{Array => _,_}
import java.util.Date

//reflection

sealed class Primitive[T]
object Primitive{
  implicit object BooleanPrimitive extends Primitive[Boolean]
  implicit object BytePrimitive extends Primitive[Byte]
  implicit object ShortPrimitive extends Primitive[Short]
  implicit object IntPrimitive extends Primitive[Int]
  implicit object LongPrimitive extends Primitive[Long]
  implicit object FloatPrimitive extends Primitive[Float]
  implicit object DoublePrimitive extends Primitive[Double]
  implicit object StringPrimitive extends Primitive[String]
  
  def anyClass[T:ClassManifest] = implicitly[ClassManifest[T]].erasure.asInstanceOf[Class[Any]]
  
  val Boolean = anyClass[Boolean]
  val Byte = anyClass[Byte]
  val Short = anyClass[Short]
  val Int = anyClass[Int]
  val Long = anyClass[Long]
  val Float = anyClass[Float]
  val Double = anyClass[Double]
  val String = anyClass[String]
}



case class DefaultValue[T](zero:T,one:T)(implicit val manifest:ClassManifest[T])
class DefaultValues(val items:DefaultValue[_ <: Any]*){
  val classMap = Map(
                   items map { x =>
                     (x.manifest.erasure.asInstanceOf[Class[Any]],x.asInstanceOf[DefaultValue[AnyRef]])
                   }:_*
                 )
                 
  def byClass[X](clazz:Class[X]) = classMap.get(clazz.asInstanceOf[Class[Any]]).getOrElse(error("unable to find default values for class type "+clazz))
}

object DefaultValues{
  implicit val defaultValues = new DefaultValues(
        DefaultValue[Boolean](false,true),
        DefaultValue[Byte](0:Byte,1:Byte),
        DefaultValue[Short](0,1),
        DefaultValue[Int](0,1),
        DefaultValue[Long](0,1),
        DefaultValue[Float](0.0f,1.0f),
        DefaultValue[Double](0.0,1.0),
        DefaultValue[String](null,"string"),
        DefaultValue[Option[_]](None,Some(null))
      )
  val items = defaultValues.items
}


case class ClassParameter(index:Int,name:String,clazz:Class[Any],classType:Type,annotations:Array[java.lang.annotation.Annotation],field:Option[Field])
class ClassDescription[T](val clazz:Class[T],defaultValues:DefaultValues){
  val fields:Array[Field] = clazz.getDeclaredFields map {x => x.setAccessible(true);x}
  val constructor:Constructor[T] = clazz.getConstructors.head.asInstanceOf[Constructor[T]]
  val parameterClasses:Array[Class[Any]] = constructor.getParameterTypes map {_.asInstanceOf[Class[Any]]}
  val parameterTypes:Array[Type] = constructor.getGenericParameterTypes
  val parameterAnnotations = constructor.getParameterAnnotations
  
  val defaults:Array[DefaultValue[AnyRef]] = parameterClasses map defaultValues.byClass
  
  def fieldByName(name:String) = fields filter {_.getName == name} headOption
  
  lazy val parameterNames = transformParameters{
    (_,item,one,index) => (for(field <- fields;value = field.get(item); if value == one) yield field.getName).head
  }
  
  lazy val parameters:Seq[ClassParameter] = for(i <- 0 until parameterNames.size)
                                              yield ClassParameter(i,parameterNames(i),parameterClasses(i),parameterTypes(i),parameterAnnotations(i),fieldByName(parameterNames(i)))

  def transformParameters[R](transform:(ClassDescription[T],T,Any,Int)=>R):Seq[R] = {
    for(paramIndex <- 0 until parameterClasses.size)
      yield transform(this,buildInstanceWithDefaults(paramIndex),defaults(paramIndex).one,paramIndex)
  } toList
  
  def buildInstanceWithDefaults(oneValues:Int*):T = {
    val args = for((default,defaultsIndex) <- defaults.zipWithIndex)
                  yield
                    if(oneValues contains defaultsIndex) default.one
                    else default.zero
                    
    newInstance(args:_*)
  }
  
  def newInstance(args:Any*) = constructor.newInstance(args.toArray.asInstanceOf[Array[Object]]:_*)
}

object ClassDescription{
  implicit def apply[T](implicit manifest:ClassManifest[T], defaultValues:DefaultValues) = new ClassDescription(manifest.erasure.asInstanceOf[Class[T]],defaultValues)
}

////sql specific

trait SqlAutoIncrement{ def value:Long }
case object SqlGeneratedId extends SqlAutoIncrement{ def value:Long = error("no value has been generated") }
case class SqlAutoIncrementValue(value:Long) extends SqlAutoIncrement{
  override def toString = value.toString
}

object SqlAutoIncrement{
  implicit def longToSqlIncrement(i:Long) = SqlAutoIncrementValue(i)
  implicit def sqlIncrementToLong(s:SqlAutoIncrement) = s.value
  
  implicit val sqlDefaultValues = new DefaultValues( (DefaultValues.items ++ Seq(DefaultValue[SqlAutoIncrement](null,SqlGeneratedId))) :_* )
}


sealed class SqlPrimitive[T]
object SqlPrimitive{
  implicit object BooleanPrimitive extends SqlPrimitive[Boolean]
  implicit object BytePrimitive extends SqlPrimitive[Byte]
  implicit object ShortPrimitive extends SqlPrimitive[Short]
  implicit object IntPrimitive extends SqlPrimitive[Int]
  implicit object LongPrimitive extends SqlPrimitive[Long]
  implicit object FloatPrimitive extends SqlPrimitive[Float]
  implicit object DoublePrimitive extends SqlPrimitive[Double]
  implicit object StringPrimitive extends SqlPrimitive[String]
  implicit object DatePrimitive extends SqlPrimitive[Date]
  implicit object SqlAutoIncrementPrimitive extends SqlPrimitive[SqlAutoIncrement]

  def anyClass[T:ClassManifest] = implicitly[ClassManifest[T]].erasure.asInstanceOf[Class[Any]]
  
  val Boolean = anyClass[Boolean]
  val Byte = anyClass[Byte]
  val Short = anyClass[Short]
  val Int = anyClass[Int]
  val Long = anyClass[Long]
  val Float = anyClass[Float]
  val Double = anyClass[Double]
  val String = anyClass[String]
  val Date = anyClass[Date]
  val SqlAutoIncrement = anyClass[SqlAutoIncrement]
}


sealed class PrimaryKey[P](val arity:Int,val primaryKeyValues: P => Seq[Any]){
  import SqlAutoIncrement.sqlDefaultValues
  def extractNames[T:ClassManifest](primaryKeyExtractor: T=>P):List[String] = {
    val desc = ClassDescription[T]
    0 until arity map { keyIndex =>
      desc.parameterNames(
        desc.transformParameters{
          (_,item,one,index) =>
          val primaryKey:P = primaryKeyExtractor(item)
          val values:Seq[Any] = primaryKeyValues(primaryKey)
          if(values(keyIndex) == one)
            Some(index)
          else
            None
        }.flatten.headOption getOrElse error("unable to find parameter name, keyIndex:"+keyIndex)
      )
    } toList
  }
}

object PrimaryKey{
  implicit def tuple1[A:SqlPrimitive] =
    new PrimaryKey[A](1,List(_))
  implicit def tuple2[A:SqlPrimitive,B:SqlPrimitive] =
    new PrimaryKey[Tuple2[A,B]](2,_.productIterator.toList)
  implicit def tuple3[A:SqlPrimitive,B:SqlPrimitive,C:SqlPrimitive] =
    new PrimaryKey[Tuple3[A,B,C]](3,_.productIterator.toList)
  implicit def tuple4[A:SqlPrimitive,B:SqlPrimitive,C:SqlPrimitive,D:SqlPrimitive] =
    new PrimaryKey[Tuple4[A,B,C,D]](4,_.productIterator.toList)
}


case class Person(id:SqlAutoIncrement,name:String,age:Int)
object People extends SqlCollection("People")({person:Person => person.id})

object SqlCollectionExamples{
  
  val database = {
    val ds = new org.h2.jdbcx.JdbcDataSource()
    ds.setURL("jdbc:h2:~/test")
    ds.setUser("sa")
    ds.setPassword("sa")
    H2Database(ds,"test")
  }
  
  def test(){
    database { implicit connection =>
      //case class MultipleKeys(owner:Int,uniqueId:SqlAutoIncrement,name:String)
      //val MultipleKeyCollection = SqlCollection("Bla"){x:MultipleKeys => (x.owner,x.uniqueId)}
      //
      ////awesome that these both work!
      //MultipleKeyCollection.retreive(0,SqlAutoIncrementValue(1))
      //MultipleKeyCollection.retreive(0,1)
      //
      //case class Awesome(id:SqlAutoIncrement, name:String)
      //val Awesomes = SqlCollection("Awesomes"){x:Awesome => x.id}
      //
      //Awesomes.store(Awesome(0,"Awesome!"))
      //Awesomes.store(Awesome(SqlGeneratedId,"Awesome!"))
      //
      //val a = Awesome(SqlGeneratedId,"Awesome!")
      //
      //a.id * 10
      //
      //Awesomes.findAll where('id === 20 and 'name === "Awesome!") delete
      //
      //val otherResults:Iterable[Awesome] = Awesomes.findAll where('id === 20 and 'name === "Awesome!")
      
      People.findAll.delete
      for(i <- 1 to 20){
        println("storing "+i)
        People store Person(SqlGeneratedId,"Person #"+i,i)
      }
      People.findAll foreach println
      println("==================")
      People.store(Person(SqlGeneratedId,"Nick",28))
      People.store(Person(12345,"Dan",28))
      println(People.retreive(12345))
      People.findAll foreach println
      println("==================")
      People.findAll where('age > 10) foreach println
      People.findAll.where('age < 5).delete
      println("==================")
      People.store(Person(12345,"Danimal",28))
      println(People.retreive(12345))
      People.findAll foreach println
      println("==================")
      People.remove(12345)
      println(People.retreive(12345))
      People.findAll foreach println
      println("==================")
      People.findAll.delete
      People.findAll foreach println
      println("==================")
    }
    
    database readOnly { implicit connection =>
      People.findAll foreach println
      println("==================")
      println(People.retreive(12345))
      People.findAll foreach println
      println("==================")
      People.findAll where('age > 10) foreach println
      println("==================")
      println(People.retreive(12345))
      People.findAll foreach println
      println("==================")
      println(People.retreive(12345))
      People.findAll foreach println
      println("==================")
      People.findAll foreach println
      println("==================")
    }
  }
  
}


class SqlCollection[T <: Product : ClassManifest,P:PrimaryKey](val tableName:String)(val primaryKeyExtractor: T=>P){
  import SqlAutoIncrement.sqlDefaultValues
  
  val classDescription = ClassDescription[T]
  val primaryKeyDefinition = implicitly[PrimaryKey[P]]
  val primaryKeyFields = primaryKeyDefinition.extractNames(primaryKeyExtractor)

  val sqlTable = SqlTable(tableName,primaryKeyFields:_*)(classDescription.parameters map parameterToColumn :_*)

  protected def parameterToColumn(param:ClassParameter):SqlColumn =  
    SqlColumn(param.name,param.clazz match {
      case SqlPrimitive.Boolean => SqlBoolColumn
      case SqlPrimitive.Byte => SqlIntColumn(SqlInt8)
      case SqlPrimitive.Short => SqlIntColumn(SqlInt16)
      case SqlPrimitive.Int => SqlIntColumn(SqlInt32)
      case SqlPrimitive.Long => SqlIntColumn(SqlInt64)
      case SqlPrimitive.Float => SqlFloatColumn(SqlFloat32)
      case SqlPrimitive.Double => SqlFloatColumn(SqlFloat64)
      case SqlPrimitive.String => SqlStringColumn(SqlVarChar(255))
      case SqlPrimitive.Date => SqlDateTimeColumn
      case SqlPrimitive.SqlAutoIncrement => SqlAutoIncrementColumn
      case clazz => error("unsupported type: "+clazz)
    })
    
  protected def validateTable(implicit connection:SqlWriteConnection){
    sqlTable.validateStructure
  }

  def retreive(primaryKey:P)(implicit connection:SqlConnection):Option[T] = {
    //validateTable
    (findAll where (primaryKeyCriteria(primaryKey)) toList) headOption
  }
  
  def store(item:T)(implicit connection:SqlWriteConnection):Unit = {
    validateTable

    val primaryKeyValues = primaryKeyDefinition.primaryKeyValues(primaryKeyExtractor(item)).filter( _ != SqlGeneratedId).collect{
      case SqlAutoIncrementValue(v) => v
      case value => value
    }

    sqlTable.insertOrUpdate(primaryKeyValues:_*){
      (row,isUpdate) =>
      
      for(param <- classDescription.parameters;field <- param.field)
        updateValue(row,param.name,field.get(item))

    }
  }
  
  def remove(item:T)(implicit connection:SqlWriteConnection):Unit =
    remove(primaryKeyExtractor(item))

  def remove(primaryKey:P)(implicit connection:SqlWriteConnection):Unit = {
    validateTable
    findAll where (primaryKeyCriteria(primaryKey)) delete    
  }
  
  def findAll(implicit connection:SqlConnection) = {
    //validateTable
    new SqlCollectionResults(this,None)
  }
  
  protected def primaryKeyCriteria(primaryKey:P) = {
    var criteria:SqlCriteria = SqlEmptyCriteria
    val primaryKeyValues = primaryKeyDefinition.primaryKeyValues(primaryKey)
    
    for( (fieldName,value) <- primaryKeyFields zip primaryKeyValues )
      criteria = criteria and equalsCriterion(fieldName,value)
    
    criteria
  }
  
  protected def updateValue(row:SqlInsert,fieldName:String,value:Any) = {
    val fieldSymbol = Symbol(fieldName)
    value match {
      case null => row.resultSet.updateNull(fieldName)
      case v:Boolean => row(fieldSymbol) = v
      case v:Byte => row(fieldSymbol) = v
      case v:Short => row(fieldSymbol) = v
      case v:Int => row(fieldSymbol) = v
      case v:Long => row(fieldSymbol) = v
      case v:Float => row(fieldSymbol) = v
      case v:Double => row(fieldSymbol) = v
      case v:String => row(fieldSymbol) = v
      case v:Date => row(fieldSymbol) = v
      case v:SqlAutoIncrementValue => row(fieldSymbol) = v.value
      case v:SqlGeneratedId.type => ()
    }
  }
  
  protected def equalsCriterion(fieldName:String,value:Any) = {
    val fieldSymbol = Symbol(fieldName)
    value match {
      case v:Boolean => fieldSymbol === v
      case v:Byte => fieldSymbol === v
      case v:Short => fieldSymbol === v
      case v:Int => fieldSymbol === v
      case v:Long => fieldSymbol === v
      case v:Float => fieldSymbol === v
      case v:Double => fieldSymbol === v
      case v:String => fieldSymbol === v
      case v:Date => fieldSymbol === v
      case v:SqlAutoIncrement => fieldSymbol === v.value
    }
  }
  
  protected[sql] def convertRowToObject(row:SqlResult):T = 
    classDescription.newInstance(
      classDescription.parameters map { param =>
        val fieldSymbol = Symbol(param.name)
        param.clazz match {
          case SqlPrimitive.Boolean => row[Boolean](fieldSymbol)
          case SqlPrimitive.Byte => row[Byte](fieldSymbol)
          case SqlPrimitive.Short => row[Short](fieldSymbol)
          case SqlPrimitive.Int => row[Int](fieldSymbol)
          case SqlPrimitive.Long => row[Long](fieldSymbol)
          case SqlPrimitive.Float => row[Float](fieldSymbol)
          case SqlPrimitive.Double => row[Double](fieldSymbol)
          case SqlPrimitive.String => row[String](fieldSymbol)
          case SqlPrimitive.Date => row[Date](fieldSymbol)
          case SqlPrimitive.SqlAutoIncrement => SqlAutoIncrementValue(row[Long](fieldSymbol))
        }
      }:_*
    )
  
}

object SqlCollection{
  def apply[T <:  Product : ClassManifest,P:PrimaryKey](tableName:String)(primaryKeyExtractor: T=>P) = 
    new SqlCollection(tableName)(primaryKeyExtractor)
}

class SqlCollectionResults[T <: Product,P](collection:SqlCollection[T,P],whereClause:Option[SqlCriteria] = None)(implicit connection:SqlConnection) extends Iterable[T]{
  def where(criteria:SqlCriteria) = {
    require(whereClause == None,"Can't specify multiple where clauses!")
    new SqlCollectionResults(collection,Some(criteria))
  }
  
  def iterator = new Iterator[T]{
    private val rawSqlResults = createSqlOps iterator
    def next = collection.convertRowToObject(rawSqlResults.next)
    def hasNext = rawSqlResults.hasNext
  }
  
  def delete(implicit connection:SqlWriteConnection) = 
    createSqlOps() delete
  
  private def createSqlOps() =
    whereClause map { criteria =>
      collection.sqlTable.findAll where (criteria)
    } getOrElse {
      collection.sqlTable.findAll
    }
}
