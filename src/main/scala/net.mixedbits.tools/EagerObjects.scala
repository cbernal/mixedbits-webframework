package net.mixedbits.tools

import java.lang.reflect.{Array => ReflectionArray,_}

trait EagerObjects{
  self =>
  
  private def objectInitializers(clazz:Class[_]):Array[Method] = 
    for{
      method <- clazz.getMethods
      returnType = method.getReturnType
      className = clazz.getName
      if method.getParameterTypes.size == 0
      if Modifier.isFinal(method.getModifiers)
      if returnType.getName startsWith className
      if returnType.getName endsWith "$"
      if returnType.getName contains method.getName
    } yield method
  
  protected def initializeObjects(instance:AnyRef = self):Unit = 
    for{
      method <- objectInitializers(instance.getClass)
      obj = method.invoke(instance)
    } initializeObjects(obj)
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
    
    initializeObjects()
    
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
