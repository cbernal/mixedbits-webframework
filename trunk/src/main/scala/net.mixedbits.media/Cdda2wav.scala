package net.mixedbits.media

import java.io._

case class Track(index:Int,startSector:Long,duration:Double)
case class Disc(cdIndexId:String,cddbId:Long,duration:Double,tracks:Seq[Track])

object Cdda2wav{
  
  class ProcessReader(command:String*){
    val process = new ProcessBuilder().directory(null).redirectErrorStream(true).command(command:_*).start()
  
    val stdin = new BufferedWriter(new OutputStreamWriter(process.getOutputStream))
    val stdout = new BufferedReader(new InputStreamReader(process.getInputStream))
    
    lazy val output = new Iterator[String]{
      var currentLine:String = _
      def next = currentLine                                         
      def hasNext = {currentLine = stdout.readLine;currentLine!=null}
    }
    
    def stop() = process.destroy()
  }
  
  def currentDevice() = "dev=/dev/cdrom"
  
  def discInfo() = {
    
    /*
    Tracks:10 47:19.16
    CDINDEX discid: HsD0mV_ODo05fcYqIuIKUdmAm6Q-
    CDDB discid: 0x7d0b170a
    CD-Text: not detected
    CD-Extra: not detected
    Album title: '' from ''
    T01:       0  1:43.60
    T02:    7785  4:12.13
    T03:   26698  3:24.17
    T04:   42015  3:39.58
    T05:   58498  6:57.69
    T06:   89842  3:07.20
    T07:  103887  4:53.09
    T08:  125871  7:04.03
    T09:  157674  5:28.60
    T10:  182334  6:48.07

    */
    var cdIndexId:String = null
    var cddbId:Long = 0
    var duration:Double = 0
    var tracks = new scala.collection.mutable.ArrayBuffer[Track]()
    
    val process = new ProcessReader("cdda2wav","-no-write","-gui",currentDevice,"-info-only","-v","toc,sectors")
    for(line <- process.output; val segments = line split "\\s+")
      if(line startsWith "CDINDEX discid:")
        cdIndexId = segments drop 2 head
      else if(line startsWith "CDDB discid:")
        cddbId = java.lang.Long.parseLong((segments drop 2 head) drop 2,16)
      else if(line startsWith "Tracks:")
        duration = Time.parseDuration(segments drop 1 head)
      else if(segments(0).startsWith("T") && segments(0).endsWith(":") && segments.size == 3)
        tracks += Track(
                    (segments(0) drop 1 dropWhile {_ == '0'} takeWhile {_ != ':'}).toInt,
                    segments(1).toLong,
                    Time.parseDuration(segments(2))
                    )
      
    //DiscMetadata(
      Disc(cdIndexId,cddbId,duration,tracks)
    //  )
  }
  
  def discMetadata() =  
    CDDB(discInfo)
  
  def ripTrack(index:Int,file:File,progress: Double=>Unit):Boolean = {
    val process = new ProcessReader("cdda2wav","-gui","-no-infofile","-paranoia",currentDevice,"track="+index,file.getAbsolutePath)
    var message = ""
    for{
      line <- process.output.dropWhile(x => !x.startsWith("percent_done:")).drop(1).takeWhile(_ contains "%")
      val (percent,remainder) = line.splitAt(line.indexOf("%"))
    } {
      progress(percent.trim.toDouble / 100.0)
      remainder.drop(1).trim match {
        case "" => 
        case value => message = value 
      }
    }

    for(line <- process.output) ()
    
    message contains "recorded successfully"
  }
}
