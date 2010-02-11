package net.mixedbits.tools

import java.io._
import java.net._
import java.util._
import java.text._
import javax.xml.parsers._
import javax.xml.xpath._
import javax.xml.transform._
import javax.xml.transform.dom._
import javax.xml.transform.stream._
import org.w3c.dom._
import org.xml.sax.{SAXException,InputSource}

object Xml{

  private lazy val xmlDocumentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder()	

  def Parse(documentFile:File) = xmlDocumentBuilder.parse(documentFile).getDocumentElement
  def Parse(inputStream:InputStream) = xmlDocumentBuilder.parse(inputStream).getDocumentElement
  def Parse(text:String) = xmlDocumentBuilder.parse(new InputSource(new StringReader(text))).getDocumentElement
  
  def ToString(node:Node) = {
    val writer = new StringWriter()
    WriteNode(node,writer)
    writer.toString
  }
  def WriteNode(node:Node, writer:Writer) = TransformerFactory.newInstance().newTransformer().transform(new DOMSource(node), new StreamResult(writer))

  def ToFormattedString(node:Node):String = ToFormattedString(node,2)

	def ToFormattedString(node:Node, indent:Int) = {
    val xmlInput = new DOMSource(node)
    val stringWriter = new StringWriter()
    val xmlOutput = new StreamResult(stringWriter)
    val transformer = TransformerFactory.newInstance().newTransformer() 
    transformer.setOutputProperty(OutputKeys.INDENT, "yes")
    transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", indent.toString)
    transformer.transform(xmlInput, xmlOutput)
    xmlOutput.getWriter().toString()
	}
  
  def Node(relativeTo:Node, xpathQuery:String) = XPathFactory.newInstance().newXPath().compile(xpathQuery).evaluate(relativeTo,XPathConstants.NODE).asInstanceOf[Node]
  def Element(relativeTo:Node, xpathQuery:String) = Node(relativeTo,xpathQuery).asInstanceOf[Element]
  
	def Nodes(relativeTo:Node, xpathQuery:String):Seq[Node] = {
    val nodes = XPathFactory.newInstance().newXPath().compile(xpathQuery).evaluate(relativeTo,XPathConstants.NODESET).asInstanceOf[NodeList]
    for(i <- 0 until nodes.getLength)
      yield nodes.item(i)
  }
  
	def Elements(relativeTo:Node, xpathQuery:String):Seq[Element] = {
    val nodes = XPathFactory.newInstance().newXPath().compile(xpathQuery).evaluate(relativeTo,XPathConstants.NODESET).asInstanceOf[NodeList]
    for(i <- 0 until nodes.getLength)
      yield nodes.item(i).asInstanceOf[Element]
  }
  
	def Text(relativeTo:Node, xpathQuery:String):String = XPathFactory.newInstance().newXPath().compile(xpathQuery).evaluate(relativeTo)
 
  
  def elem(name:String) = scala.xml.Elem(null,name,null,scala.xml.TopScope,Nil:_*)
  def attribute(name:String,value:String) = new scala.xml.UnprefixedAttribute(name,value,scala.xml.Null)
  def attributes(values:(String,String)*):scala.xml.MetaData = {
    var current:scala.xml.MetaData = null
    for( (key,value) <- values ){
      if(current == null)
        current = attribute(key,value)
      else
        current = current.append(attribute(key,value))
    }
    current
  }
  
  def attributes(elem:scala.xml.Elem)(values:(String,String)*):scala.xml.Elem = 
    if(values.size > 0)
      elem % attributes(values:_*)
    else
      elem
  
}
