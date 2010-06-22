package net.mixedbits.webframework

import scala.xml._

object Tools{
  type Elements = NodeBuffer
  implicit def elemToNodeBuffer(element:Elem) = 
    new NodeBuffer() &+ element
  
  object Elements{
    def apply(elements:Seq[Node]) = {
      val buffer = new NodeBuffer
      for(e <- elements)
        buffer &+ e
      buffer
    }
    def apply(elements:Elements):Elements = elements
    def apply():Elements = new Elements
    
    def toXhtml(elem:Elem) = Xhtml.toXhtml(elem)
    def toXhtml(elements:Elements) = {
      val buffer = new StringBuffer
      
      for(elem <- elements)
        buffer.append(Xhtml.toXhtml(elem))
      
      buffer.toString
    }
  }
}
