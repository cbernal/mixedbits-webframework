package net.mixedbits.vfs

import java.io._
import net.mixedbits.tools._

trait Path extends Comparable[Path]{
  self =>
  val storageProvider:StorageProvider
  val vfs:VirtualFileSystem
  val rawPath:String //never allow to have a trailing or preceding / :D
  
  def list():Array[Path]
  
  def isDirectory:Boolean = exists && !isFile
  def isFile:Boolean
  
  def mkdir():Unit
  
  def copyTo(dest:OutputStream):Unit =
    for(source <- use(openRead))
      IO.pipeStream(source,dest)
  
  def copyTo(dest:File):Unit = 
    for(output <- use(new FileOutputStream(dest)))
      copyTo(output)
  
  def copyTo(dest:Path):Unit = 
    if(!dest.exists)
      for(output <- use(dest.openWrite))
        copyTo(output)
    else
      error("destination path already exists!")
  
  def openWrite():OutputStream
  def openRead():InputStream
  
  def size:Long
  def lastModified:Long
  
  def name:String = rawPath.substring(rawPath.lastIndexOf("/")+1,rawPath.length)
    
  def exists:Boolean
  
  def ensureExists():Path = {
    Option(parent) foreach {_.ensureExists}
    
    if(!exists)
      mkdir()
    
    this
  }
  
  def child(subPath:String):Path = 
    vfs.path(rawPath + "/" + vfs.normalize(subPath))
  
  def / (subPath:String):Path = child(subPath)
  
  def parent:Path = {
    val lastSlash = rawPath.lastIndexOf("/")
    if(lastSlash == -1)
      null
    else
      vfs.path(rawPath.substring(0,lastSlash))
  }
  
  def parents =
    new Iterator[Path]{
      var current = self
      def hasNext = {
        if(current != null)
          current = current.parent
        
        current != null
      }
      def next = current
    }
  
  def delete()
  
  def deleteAll(){
    list foreach {_.deleteAll}
    delete
  }

  override def equals(obj:Any) = obj match {
    case p:Path => p.vfs == vfs && p.rawPath == rawPath
    case _ => false
  }
  
  override def hashCode = rawPath.hashCode
  def compareTo(path:Path) = rawPath compareTo path.rawPath
  
  def readAllText:String = IO.readAllText(openRead)
  def readAllText(encoding:String) = IO.readAllText(openRead,encoding)
  
  override def toString() = "Path("+rawPath+")"
}
