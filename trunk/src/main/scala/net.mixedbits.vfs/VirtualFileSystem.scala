package net.mixedbits.vfs

class VirtualFileSystem(_storageProviders:(String,StorageProvider)*){
  //store the providers list in order from longest to shortest prefix
  val providers = _storageProviders map {case (path,provider) => (normalize(path),provider)} sortBy {-_._1.length}
  
  private def providerFor(path:String):Option[StorageProvider] = 
    {for((prefix,provider) <- providers if path startsWith prefix) yield provider}.headOption
  
  private def openPath(path:String) = providerFor(path) map {_.path(path,this)} getOrElse error("No provider found for path: "+path)
  
  def normalize(path:String) = path match {
    case "/" => ""
    case _ => path.drop(if(path startsWith "/") 1 else 0).dropRight(if(path endsWith "/") 1 else 0)
  }
  
  def path(rawPath:String):Path = openPath(normalize(rawPath))
  def / (rawPath:String):Path = path(rawPath)
}
