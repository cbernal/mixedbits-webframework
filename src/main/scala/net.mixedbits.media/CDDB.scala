package net.mixedbits.media

object CDDB extends (RawDisc => Seq[DiscResponse]){
  
  private def runCommand(cmd:String) = new Iterator[String]{
    val reader = new java.io.BufferedReader(
                  new java.io.InputStreamReader(
                    new java.net.URL("http://freedb.freedb.org/~cddb/cddb.cgi?cmd="+cmd.replaceAll(" ","+")+"&hello=a+b+c+1.0&proto=6")
                        .openConnection()
                        .getInputStream()
                    )
                  )
    var currentLine:String = _
    def next = currentLine                                         
    def hasNext = {currentLine = reader.readLine;currentLine!=null}
  }.toList
  
  def apply(info:RawDisc):List[DiscResponse] = {
    import info._
    val cddbDiscId = java.lang.Long.toString(cddbId,16)
    
    runCommand("cddb query "+cddbDiscId+" "+tracks.size+" "+tracks.map(_.startSector).mkString(" ")+" "+duration.toInt) match {
      case head :: tail if head startsWith "200" =>
        readAlbum(info,head drop 4) :: Nil
      case head :: tail if head startsWith "210" =>
        tail takeWhile {_!="."} map {readAlbum(info,_)}
      case _ =>
        Nil
    }
  }
  
  def debug(s:String) = () 
  
  def readAlbum(info:RawDisc,line:String) = {
    var genre = line takeWhile {_!=' '}
    val id = line drop genre.size+1 takeWhile {_!=' '}
    val remainder = line drop genre.size+id.size+2
    val Array(artist,album) = remainder split '/'

    debug("artist: "+artist.trim)
    debug("album: "+album.trim)
    
    var year:String = null
    val tracks = new scala.collection.mutable.ArrayBuffer[(String,String)]()
    
    runCommand("cddb read "+genre+" "+id) match {
      case head :: tail if head startsWith "210" =>

        for{
          line <- tail
          if !line.startsWith("#")
          if line contains "="
          (key,remainder) = line splitAt line.indexOf('=')
          value = remainder drop 1
        } key match{
          case "DYEAR" =>
            year = value
            debug("year: "+value)
          case "DGENRE" =>
            genre = value
            debug("genre: "+value)
          case key if key startsWith "TTITLE" =>
            tracks += (key drop 6) -> value
            debug("track("+((key drop 6).toInt+1)+"): "+value)
          case _ =>
        }
      
      case _ =>
        println("no entry found")
    }
      
    DiscResponse(
      info,
      for( ((number,title),rawTrack) <- tracks.toList.zip(info.tracks))
        yield TrackResponse(number.toInt+1,title,year,genre,artist.trim,album.trim,null,rawTrack.duration)
    )
  }

    /*
    REQUEST:
      cddb+query+7d0b170a+10+0+7785+26698+42015+58498+89842+103887+125871+157674+182334+2839
    RESPONSE:
      200 blues 7d0b170a The Bakerton Group / El Rojo
    
    REQUEST: 
      cddb+query+b10a7a0d+13+150+15525+37143+54917+69880+84710+96537+111848+130336+142690+149517+167132+183816+2684
    RESPONSE:
      210 Found exact matches, list follows (until terminating `.')
      folk b10a7a0d Kaiser Chiefs / Yours Truly Angry Mob
      misc b10a7a0d Kaiser Chiefs / Yours Truly, Angry Mob
      .

    
    REQUEST:
      cddb+read+blues+7d0b170a
    RESPONSE:  
      210 blues 7d0b170a CD database entry follows (until terminating `.')
      # xmcd
      #
      # Track frame offsets: 
      #        150
      #        7935
      #        26848
      #        42165
      #        58648
      #        89992
      #        104037
      #        126021
      #        157824
      #        182484
      #
      # Disc length: 2841 seconds
      #
      # Revision: 0
      # Processed by: cddbd v1.5.2PL0 Copyright (c) Steve Scherf et al.
      # Submitted via: ExactAudioCopy v0.95b4
      #
      DISCID=7d0b170a
      DTITLE=The Bakerton Group / El Rojo
      DYEAR=2009
      DGENRE=Rock
      TTITLE0=Time Horizon
      TTITLE1=Chancellor
      TTITLE2=Peruvian Airspace
      TTITLE3=Bien Clásico
      TTITLE4=Life On Lars
      TTITLE5=M.(f).H.S.
      TTITLE6=The Gigantomakhia
      TTITLE7=Work'em
      TTITLE8=Last Orbit
      TTITLE9=Bill Proger's Galaxy
      EXTD=
      EXTT0=
      EXTT1=
      EXTT2=
      EXTT3=
      EXTT4=
      EXTT5=
      EXTT6=
      EXTT7=
      EXTT8=
      EXTT9=
      PLAYORDER=
      .
      */
  
  //hello username hostname clientname version
  //http://ftp.freedb.org/pub/freedb/latest/CDDBPROTO
  
  
}
