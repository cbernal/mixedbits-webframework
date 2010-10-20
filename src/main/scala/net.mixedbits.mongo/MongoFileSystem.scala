package net.mixedbits.mongo

import net.mixedbits.tools._
import net.mixedbits.json._
import net.mixedbits.tools._
import com.mongodb._
import com.mongodb.gridfs._
import java.io._

trait MongoFileProperties{
  
  object Path extends JsStringProperty("filename")
  
  object MD5 extends JsStringProperty("md5")
  object Length extends JsIntProperty("length")
  object CreationDate extends JsDateProperty("uploadDate")
  object ContentType extends JsStringProperty("contentType")
  
  /*
  maybe add support for some sort of content type groups,
    ie image/jpeg and image/png would be in the image group,
    and text/plain and text/html would be in the text group
  */
  
  object Metadata extends JsObjectProperty("metadata")
  
  //object PathData extends JsObjectProperty(Metadata,"pathdata")
  object Filename extends JsStringProperty(Metadata,"filename")
  object Folder extends JsStringProperty(Metadata,"folder")
  object SimpleName extends JsStringProperty(Metadata,"simpleName")
  object Extension extends JsStringProperty(Metadata,"extension")
}

object MongoFileChunk{
  object FileId extends JsAnyProperty("files_id")
  object ChunkNumber extends JsAnyProperty("n")
  object Data extends JsAnyProperty("data")
}

class MongoFileSystem(databaseReference: => MongoDatabase, name:String) extends MongoBaseCollection[MongoFile]{
  def this(database: => MongoDatabase) = this({database}, null)
  
  val collectionName:String =
    if(name == null)
      Objects.simpleClassName(this)
    else
      name
    
  lazy val database = databaseReference
  lazy val rawFileSystem = new GridFS(database.getDatabase,collectionName)
  lazy val metadata = new MongoCollection(database,collectionName+".files")
  lazy val chunks = new MongoCollection(database,collectionName+".chunks")
  
  def index(indexName:String,properties:(JsProperty[_],SortDirection)*) = 
    metadata.usingWriteConnection{MongoTools.ensureIndex(_,Option(indexName),properties:_*)}
  
  def store(source:java.io.File, path:String, updates:JsUpdate) = storeFile(rawFileSystem.createFile(source),path,Some(updates))
  def store(source:java.io.File, path:String) = storeFile(rawFileSystem.createFile(source),path,None)
  
  def store(source:java.io.InputStream, path:String, updates:JsUpdate) = storeFile(rawFileSystem.createFile(source),path,Some(updates))
  def store(source:java.io.InputStream, path:String) = storeFile(rawFileSystem.createFile(source),path,None)
  
  private def storeFile(inputFile:gridfs.GridFSInputFile, path:String, updates:Option[JsUpdate]) = {
    //remove any existing files with this path...
    removeFile(path)
    
    val normalizedPath = MongoFileSystem.normalizePath(path)
    val (folder,filename) = MongoFileSystem.splitParts(normalizedPath)
    
    //inputFile.put("_id",normalizedPath) //doing this ensures single entries for metadata, but leaves orphaned chunks :(
    inputFile.setFilename(normalizedPath)
    inputFile.setContentType("") //detect some sort of content type... 
    inputFile.getMetaData
    
    //store all common metadata
    val fileData = new JsObject(inputFile)
    fileData(Filename) = filename
    fileData(Folder) = folder
    fileData(Extension) = MongoFileSystem.fileExtension(filename)
    fileData(SimpleName) = MongoFileSystem.simpleName(filename)
    
    //apply the requested updates if they exist
    for(update <- updates)
      fileData(update)

    inputFile.save

    new MongoFileCreateResult(inputFile.get("_id").toString,path,this)
  }
  
  def count() = metadata.count
  
  def findOne(constraint:JsConstraint):Option[MongoFile] = 
    Option(rawFileSystem.findOne(constraint.buildSearchObject))
      .map(new MongoFile(_,database))
  
  def findAll():MongoFileSystemResultSet = 
    new MongoFileSystemResultSet(this,None)
  
  def find(constraint:JsConstraint):MongoFileSystemResultSet = 
    new MongoFileSystemResultSet(this,Some(constraint))

  def listFiles(folder:String):MongoFileSystemResultSet =
    find(Folder == folder)
  //  rawFileSystem.find(new BasicDBObject("metadata.folder",MongoFileSystem.normalizePath(folder)+"/"))
  
  def listFiles(folder:String,extension:String):MongoFileSystemResultSet =
    find(Folder == folder and Extension == extension)
  //  rawFileSystem.find(new BasicDBObject("metadata",new BasicDBObject("folder",MongoFileSystem.normalizePath(folder)+"/").append("extension",extension)))
  
  def exists(path:String):Boolean = 
    getFile(path).isDefined

  def getFile(path:String):Option[MongoFile] = 
    Option(rawFileSystem.findOne(MongoFileSystem.normalizePath(path)))
      .map(new MongoFile(_,database))
      
  def readFile(path:String):Option[java.io.InputStream] =
    getFile(path).map(_.inputStream)
  
  def readTextFile(path:String):Option[String] = 
    getFile(path).map(_.readAllText)
  
  def readTextFile(path:String,encoding:String):Option[String] = 
    getFile(path).map(_.readAllText(encoding))
  
  def removeById(id:String){
    rawFileSystem.remove(new JsDocument(id).obj)
  }
  def remove(doc:MongoFile) = removeById(doc.id)
  
  def removeFile(path:String) = 
    rawFileSystem.remove(path)
  
  def updateChunksByFileId(id:String)(updates:JsUpdate) = 
    chunks.find(MongoFileChunk.FileId == JsTools.marshalId(id)).update(updates)

  object Metadata extends JsObjectProperty("metadata")
  
  //object PathData extends JsObjectProperty(Metadata,"pathdata")
  object Filename extends JsStringProperty(Metadata,"filename")
  object Folder extends JsStringProperty(Metadata,"folder")
  object SimpleName extends JsStringProperty(Metadata,"simpleName")
  object Extension extends JsStringProperty(Metadata,"extension")
}

object MongoFileSystem{
  //path helpers
  def buildPath(part1:String, parts:String*) =
    normalizePath(part1+"/"+parts.mkString("/"))

  def fileExtension(name:String) = 
    if(name contains ".")
      name.substring(name.lastIndexOf(".")+1,name.length)
    else
      ""
    
  def normalizePath(path:String) = {
    def ensureRooted(path:String) = 
      if(!path.startsWith("/"))
        "/"+path
      else
        path
      
    def ensureUnix(path:String) = 
      if(path contains """\""")
        path.replace("""\""","/")
      else
        path
      
    def removeTrailingSlash(path:String) = 
      if(path endsWith "/")
        path.substring(0,path.length-1)
      else
        path
      
    def removeDoubleSlashes(path:String) = {
      var result = path
      while(result contains "//")
        result = result.replace("//","/")
      result
    }
      
      
    removeDoubleSlashes(ensureRooted(removeTrailingSlash(ensureUnix(path))))
    }
    
  def splitParts(path:String):(String,String) = {
    val index = path.lastIndexOf("/")
    ( path.substring(0,index+1), path.substring(index+1,path.length) )
  }
  
  def simpleName(name:String) = {
    val lastDot = name.lastIndexOf(".")
    if(lastDot == -1)
      name
    else
      name.substring(0,lastDot)
  }
}

class MongoFileSystemResultSet(filesystem:MongoFileSystem,constraint:Option[JsConstraint]) extends MongoResultSet[MongoFile](filesystem.metadata,constraint) with MongoUpdatableResultSet[MongoFile]{
  override protected lazy val cursor = 
    filesystem.rawFileSystem.getFileList(constraintToDBObject)

  lazy val setGridFS = {
    val method = classOf[GridFSFile].getDeclaredMethod("setGridFS",classOf[GridFS])
    method.setAccessible(true)
    (obj:GridFSDBFile,fs:GridFS) => {method.invoke(obj,fs);obj}
  }

  protected def convertRawObject(rawObject:DBObject) = 
    new MongoFile(setGridFS(rawObject.asInstanceOf[GridFSDBFile],filesystem.rawFileSystem),filesystem.database)
  
  def update(updates:JsUpdate):Long =
    updateCollection(filesystem.metadata)(updates)
  
  def updateFirst(updates:JsUpdate):Boolean =
    updateCollectionFirst(filesystem.metadata)(updates)
  
  def remove() = 
    filesystem.rawFileSystem.remove(constraintToDBObject)
}

class MongoFileCreateResult(val id:String,val filename:String,fileSystem:MongoFileSystem){
  lazy val file = fileSystem.getFile(filename)
}

class MongoFile(baseObject:GridFSDBFile,val database:MongoDatabase) extends JsDocument(baseObject){
  private lazy val pathParts = MongoFileSystem.splitParts(path)
  def folder() = pathParts._1
  def name() = pathParts._2
  def simpleName = MongoFileSystem.simpleName(name)
  def extension() = MongoFileSystem.fileExtension(name)
  def path() = baseObject.getFilename()
  def length() = size
  def size() = baseObject.getLength
  def inputStream() = baseObject.getInputStream
  def bufferedStream() = new BufferedInputStream(inputStream)
  def readAllText() = IO.readAllText(inputStream)
  def readAllText(encoding:String) = IO.readAllText(inputStream,encoding)
  
  def contentType():String = 
    this(MongoFile.ContentType) filter {_ != ""} getOrElse MimeTypes.getContentTypeForName(name)
}

object MongoFile extends MongoFileProperties
