package net.mixedbits.webframework

import scala.xml._

object Tools{
  type Elements = NodeBuffer
  implicit def elemToNodeBuffer(element:Elem) = 
    new NodeBuffer() &+ element
  
  def Elements(elements:Elements):Elements = elements
  
  def Elements():Elements = new Elements
}
