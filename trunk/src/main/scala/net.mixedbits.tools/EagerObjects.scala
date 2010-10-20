package net.mixedbits.tools

import java.lang.reflect.{Array => ReflectionArray,_}

trait EagerObjects{
  self =>
  
  private def objectInitializers(clazz:Class[_]):Array[Method] = {
    val className = clazz.getName
    for{
      method <- clazz.getMethods
      returnType = method.getReturnType
      if method.getParameterTypes.size == 0
      if Modifier.isFinal(method.getModifiers)
      if returnType.getName startsWith className
      if returnType.getName endsWith "$"
      if returnType.getName contains method.getName
    } yield method
  }
  
  private def initializeObjects(instance:Any,clazz:Class[_]):Unit = 
    for(method <- objectInitializers(clazz)){
      val obj = method.invoke(instance)
      initializeObjects(obj,obj.getClass)
    }
  
  protected def initializeObjects:Unit = initializeObjects(self,self.getClass)
}


object EagerObjects{

  class E extends EagerObjects{
    var objOk = false
    var innerOk = false
    var clazzOk = true
    object Obj{
      objOk = true
      object Inner{
        innerOk = true
      }
    }
    class Clazz{
      clazzOk = false
    }                        
    final def Clazz = {
      println("Clazz called...")
      new Clazz
    }
    
    initializeObjects
    
    if(!(objOk && innerOk && clazzOk))
      error("eager initialization error!")
  }
  
  class Y{
    var a = false
    object Inner{
      a = true
    }
  }
  
  def test(){
    new E()
  }
}

