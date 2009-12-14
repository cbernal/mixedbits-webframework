package net.mixedbits.webframework

case class WebPathMatch(pattern:String,url:String,wildcards:String*)

class WebPath(private val parent:WebPath,private val path:Symbol){
  def this(parent:WebPath,path:String) = this(parent,Symbol(path))
  def / (subPath:Symbol) = {new WebPath(this,subPath)}
  def / (subPath:String) = {new WebPath(this,subPath)}
  
  lazy val segments:List[WebPath] = 
    if(parent == null)
      List(this)
    else
      parent.segments ++ List(this) 
  
  private lazy val pathValue = path match {
    case Symbol("") => ""
    case Symbol(value) => "/"+value
  }
    
  lazy val name = Symbol.unapply(path).getOrElse("")
  
  override def toString = segments.map(_.pathValue).mkString
  
  def testPath(path:String):Option[WebPathMatch] = {
    var parts = path split '/'
    var wildcards:List[String] = Nil
    
    //deal with empty root path
    if(parts.length == 0 && this == WebPath)
      return Some(WebPathMatch(toString,path))

    //deal with mismatched path lengths
    if(segments.length != parts.length)
      return None
    
    //deal with all other cases
    for( (part,segment) <- parts zip segments.toArray )
      if(segment.name == "*")
        wildcards = wildcards ++ List(part)
      else if(part != segment.name)
        return None
      
    return Some(WebPathMatch(toString,path,wildcards:_*))
  }
}

object WebPath extends WebPath(null,""){
  def Wildcard = Symbol("*")
  def apply(value:String):WebPath = {
    var current:WebPath = WebPath
    for(segment <- value split '/') segment match{
      case "" => current
      case _ => current = current / Symbol(segment)
    }
    current
  }
}

