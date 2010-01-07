package net.mixedbits.tools

import scala.tools.nsc.util.NameTransformer._
 

//borrowed from http://gist.github.com/raw/110323/e7ce72633bea69abdfd54e437845bbba51e0704a/reflection.scala
//no license mentioned, so if it turns out to be non-compatibile, I'll just remove this class

object Reflection {
  implicit def anyToReflectionMethods(x: Any) = new ReflectionMethods(x)
}
 
class ReflectionMethods(x: Any) {
  def methods_ = println(methods.reduceLeft[String](_ + ", " + _))
  def methods__ = methods.foreach(println _)
  def fields_ = println(fields.reduceLeft[String](_ + ", " + _))
  def fields__ = fields.foreach(println _)
  
  def methods = wrapped.getClass
      .getDeclaredMethods
      .toList
      .map(m => decode(m.toString
                        .replaceFirst("\\).*", ")")
                        .replaceAll("[^(]+\\.", "")
                        .replace("()", "")))
      .filter(!_.startsWith("$tag"))
  
  def fields = wrapped.getClass
      .getDeclaredFields
      .toList
      .map(m => decode(m.toString.replaceFirst("^.*\\.", "")))
 
  private def wrapped: AnyRef = x match {
    case x: Byte => byte2Byte(x)
    case x: Short => short2Short(x)
    case x: Char => char2Character(x)
    case x: Int => int2Integer(x)
    case x: Long => long2Long(x)
    case x: Float => float2Float(x)
    case x: Double => double2Double(x)
    case x: Boolean => boolean2Boolean(x)
    case _ => x.asInstanceOf[AnyRef]
  }
}
