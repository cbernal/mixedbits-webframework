package net.mixedbits.tools


import java.io.File

class FileWatcher(files: =>Seq[File]){
  
  private def cacheModifiedTimes(files:Seq[File]) = 
    Map(files.map(f=> (f,f.lastModified)):_*)
    
  private var oldModifiedTimes = Map[File,Long]() 
  
  private def findModifiedFiles(modifiedTimes:Map[File,Long],newFiles:Seq[File]):Seq[File] = {
    for(
      file <- newFiles;
      //check for new or modified files
      if !(modifiedTimes contains file) ||
          modifiedTimes(file) < file.lastModified    
      ) yield file
  }
  
  def currentFiles():Seq[File] = {
    oldModifiedTimes.keySet.toList
  }

  def modifiedFiles():Seq[File] = {
    val newFiles = files
    val modifiedTimes = oldModifiedTimes
    oldModifiedTimes = cacheModifiedTimes(newFiles)
    findModifiedFiles(modifiedTimes,newFiles)
  }
  
  def hasUpdates() = {
    val newFiles = files
    val modifiedTimes = oldModifiedTimes
    oldModifiedTimes = cacheModifiedTimes(newFiles)  
    findModifiedFiles(modifiedTimes,newFiles).size > 0
  }
  
  def isUpToDate() = 
    findModifiedFiles(oldModifiedTimes,files).size == 0
  
  def readUpdates(){ hasUpdates }

}
