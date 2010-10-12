package net.mixedbits.tools

class Dynamic private(val dynamicTarget:AnyRef,name:String = null){
  import java.lang.reflect.{Array=>_,_}
  
  def findMatchingMethod(methods:Array[Method],name:String,argClasses:Array[Class[_]]):Method = {
    val names = if(name == null) List("apply","invoke") else List(name)
    val viableOptions = methods filter {names contains _.getName} filter {_.getParameterTypes.size == argClasses.size}
    //println(viableOptions mkString "\n")
    val result = (viableOptions filter {method => classListsMatch(method.getParameterTypes,argClasses)}).headOption
    result getOrElse error("unable to find matching method: "+name+argClasses.mkString("(",",",")"))
  }
  
  def classListsMatch(a:Array[Class[_]],b:Array[Class[_]]):Boolean = 
    a zip b forall {case (x,y) => classesMatch(x,y)}
  
  def classesMatch(methodClass:Class[_],argClass:Class[_]):Boolean = {
    if(methodClass == argClass)
      true
    else if(methodClass isAssignableFrom argClass)
      true
    else if(methodClass.isPrimitive || argClass.isPrimitive)
      primitiveClassesMatch(methodClass,argClass)
    else
      false
  }
  
  def primitiveClassesMatch(methodClass:Class[_],argClass:Class[_]):Boolean = {
    val a = if(methodClass.isPrimitive) methodClass else argClass
    val b = if(methodClass.isPrimitive) argClass else methodClass
    val classes = (a,b)
    
    if(classes == (classOf[Boolean],classOf[java.lang.Byte])) true
    else if (classes == (classOf[Char],classOf[java.lang.Character])) true
    else if (classes == (classOf[Byte],classOf[java.lang.Byte])) true
    else if (classes == (classOf[Short],classOf[java.lang.Short])) true
    else if (classes == (classOf[Int],classOf[java.lang.Integer])) true
    else if (classes == (classOf[Long],classOf[java.lang.Long])) true
    else if (classes == (classOf[Float],classOf[java.lang.Float])) true
    else if (classes == (classOf[Double],classOf[java.lang.Double])) true
    else false    
  }
  
  def callMethod(target:AnyRef,name:String,args:Seq[Any]) = {
    //println("Invoking: "+name+args.mkString("(",",",")"))
    val clazz = target.getClass
    val mappedArgs = args map {_.asInstanceOf[AnyRef]}
    val argClasses = mappedArgs map {_.getClass}
    
    //we can't use the normal reflection method resolution process
    //we have to manually search so that we can resolve reference / primitive mismatch
    findMatchingMethod(clazz.getMethods,name,argClasses.toArray).invoke(target,mappedArgs:_*)
  }
  
  def ~> (methodName:Symbol):Dynamic =
    if(name == null)
      Dynamic(dynamicTarget,methodName.name)
    else
      Dynamic(~>(),methodName.name)
  
  def ~> (args:Any*):Dynamic = 
    if(name == null)
      Dynamic(callMethod(dynamicTarget,null,args))
    else
      Dynamic(callMethod(dynamicTarget,name,args))
    
  def ~ (args:Any*):Dynamic = ~>(args:_*)
  
  def resolvedTarget():Any = 
    if(name == null)
      dynamicTarget
    else
      ~>().dynamicTarget
    
  def as[T] = 
    resolvedTarget().asInstanceOf[T]

  override def toString =
    resolvedTarget().toString
}

object Dynamic{
  
  def apply(x:Any,name:String = null):Dynamic = x match {
    case null => null
    case d:Dynamic => new Dynamic(d.dynamicTarget,name)
    case x => new Dynamic(x.asInstanceOf[AnyRef],name)
  }
  
  implicit def anyToDynamic(x:Any) = Dynamic(x)
  implicit def dynamicToAny[T](x:Dynamic) = x.as[T]
}
