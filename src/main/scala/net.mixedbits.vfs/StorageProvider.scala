package net.mixedbits.vfs

import java.io._

trait StorageProvider{
  def path(rawPath:String,vfs:VirtualFileSystem):Path
}

class LocalStorageProvider(root:File) extends StorageProvider{
  self =>
  
  def path(rawPath:String,vfs:VirtualFileSystem) = {
    val storageProvider = self
    val _rawPath = rawPath
    val _vfs = vfs
    val asFile = new File(root,rawPath)
    new Path{
      val storageProvider = self
      val rawPath = _rawPath
      val vfs = _vfs
        
      def list():Array[Path] = (Option(asFile.list) getOrElse Array()) map child
      def isFile:Boolean = asFile.isFile
      
      def mkdir() =
        if(!exists && !asFile.mkdir)
          error("unable to create directory: "+rawPath)
      
      def openWrite():OutputStream = new FileOutputStream(asFile)
      def openRead():InputStream = new FileInputStream(asFile)
      
      def size:Long = asFile.length
      def lastModified:Long = asFile.lastModified
        
      def exists:Boolean = asFile.exists
      
      def delete() =
        if(exists && !asFile.delete)
          error("unable to delete file: "+rawPath)
    
      override def toString() = "LocalPath("+rawPath+" -> "+asFile.getAbsolutePath+")"
    }
  }
  
}
