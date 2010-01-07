package net.mixedbits.mongo

import net.mixedbits.tools._
import net.mixedbits.tools.Objects._
import net.mixedbits.tools.BlockStatements._
import com.mongodb._

class MongoFileSystem(database:MongoDatabase, name:String){
  def this(database:MongoDatabase) = this(database,null)
  val collectionName:String =
    if(name == null)
      Objects.simpleClassName(this)
    else
      name
  lazy val fs = new gridfs.GridFS(database.getDatabase)
  
  def store(folder:String, name:String, source:java.io.File) = storeFile(folder,name,fs.createFile(source))
  def store(folder:String, name:String, source:java.io.InputStream) = storeFile(folder,name,fs.createFile(source))
  
  private def storeFile(folder:String, name:String, inputFile:gridfs.GridFSInputFile){
    inputFile.setFilename(combinePaths(folder,name))
    inputFile.getMetaData.put("folder",normalizeFolderPath(folder))
    inputFile.getMetaData.put("extension",fileExtension(name))
    inputFile.setContentType("")
    inputFile.save
  }

  def listFiles() = 
    fs.find(new BasicDBObject())  
  
  def listFiles(folder:String) = 
    fs.find(new BasicDBObject("metadata.folder",folder))
  
  def listFiles(folder:String,extension:String) = 
    fs.find(new BasicDBObject("metadata",new BasicDBObject("folder",folder).append("extension",extension)))
  
  def readFile(folder:String, name:String):Option[java.io.InputStream] =
    Objects.toOption(fs.findOne(combinePaths(folder,name)))
      .map(_.getInputStream)
  
  private def normalizeFolderPath(folder:String) = 
    if(folder endsWith "/")
      folder.substring(0,folder.length-1)
    else
      folder
    
  private def combinePaths(folder:String, name:String) = 
    normalizeFolderPath(folder)+"/"+name

  private def fileExtension(name:String) = 
    if(name contains ".")
      name.substring(name.lastIndexOf(".")+1,name.length)
    else
      ""
}
