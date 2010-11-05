package net.mixedbits.tools

import scala.xml._
import scala.xml.parsing.XhtmlEntities

object XhtmlTest{
  val x = true
  val title = "title..."
  val body = 
    <body>
      <h1>Hola!</h1>
      collapsed img: <img />
      collapsed hr: <hr />
      uncollapsed div: <div></div>
      collapsed div: <div/>
      collapsed span: <span/>
      collapsed anchor: <a/>
      Some:{Some(<a>element inside of Some</a>)}
      Some:{Some("text inside of Some")}
      Some:{Some(true)}
      Some:{Some(1.0)}
      Some:{Some(1)}
      None:{None}
      ():{()}
      if(true):{if(x == true) "x == true"}
      if(false):{if(x != true) "x != true"}
      null:{null}
    </body>
  val testElement = 
      <html xmlns="http://www.w3.org/1999/xhtml" lang="en_US" xml:lang="en_US">
        <head>
          <title>{title}</title>
          <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        </head>
        {body}
      </html>
      
      
  def apply(){
    println(ToXhtml(testElement))
  }
}

object ToXhtml{
  case class ConversionSettings(stripComments:Boolean,decodeEntities:Boolean,preserveWhitespace:Boolean,minimizeTags:Boolean)
  val defaultConversionSettings = ConversionSettings(false,false,false,true)
  private val minimizableElements = List("base", "meta", "link", "hr", "br", "param", "img", "area", "input", "col")
  
  def apply(node:Node):String = {
    val buffer = new StringBuilder
    apply(node,buffer,TopScope,defaultConversionSettings)
    buffer.toString
  }
  
  def apply(node:Node,buffer:StringBuilder,pscope:NamespaceBinding,settings:ConversionSettings){
    import settings._
    def decode(entity: EntityRef) = XhtmlEntities.entMap.get(entity.entityName) match {
      case Some(c) if c.toInt >= 128  => buffer.append(c)
      case _                          => entity.buildString(buffer)
    }
    def shortForm = 
      minimizeTags &&
      (node.child == null || node.child.length == 0) && 
      (minimizableElements contains node.label)
    
    node match {
      case comment: Comment                     => if (!stripComments) comment buildString buffer
      case entity: EntityRef if decodeEntities  => decode(entity)
      case group: Group                         => group.nodes foreach { apply(_, buffer, node.scope, settings) }
      case atom:Atom[_]                         =>
        atom.data match {
          case None =>
          case Some(value) => value match {
            case n:Node => apply(n, buffer, node.scope, settings) 
            case other => Utility.escape(other.toString(), buffer)
          }
          case _ => atom buildString buffer
        }
      case special: SpecialNode                 => special buildString buffer
      case _  =>
        buffer.append('<')
        node.nameToString(buffer)
        
        if (node.attributes ne null)
          node.attributes.buildString(buffer)
        
        node.scope.buildString(buffer, pscope)
        
        if (shortForm) buffer.append(" />")
        else {
          buffer.append('>')
          apply(node.child, buffer, node.scope, settings)
          buffer.append("</")
          node.nameToString(buffer)
          buffer.append('>')
        }
    }
  }
  
  def apply(children: Seq[Node]):String = {
    val buffer = new StringBuilder
    apply(children,buffer,TopScope,defaultConversionSettings)
    buffer.toString
  }
  def apply(children: Seq[Node],buffer:StringBuilder,pscope:NamespaceBinding,settings:ConversionSettings){
    if (children.isEmpty)
      return
      
    val doSpaces = children forall isAtomAndNotText // interleave spaces
    for (child <- children.take(children.length - 1)) {
      apply(child, buffer, pscope, settings)
      if (doSpaces) buffer append ' '
    }
    apply(children.last, buffer, pscope, settings)
  }
  
  //duplicated from https://lampsvn.epfl.ch/trac/scala/browser/scala/trunk/src/library/scala/xml/Utility.scala
  private def isAtomAndNotText(x: Node) = x.isAtom && !x.isInstanceOf[Text]
}
