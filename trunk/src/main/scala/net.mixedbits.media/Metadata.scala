package net.mixedbits.media

trait Track{
  val number:Int
  val title:String
  val year:String
  val genre:String
  val artist:String
  val album:String
  val albumArtist:String
  val duration:Double
  
  protected def validate(values:String*) = (values map Option.apply flatten) filter {_.trim != ""} headOption
  def simpleName = List(
                        validate(artist,albumArtist),validate(album),
                        Option(number) filter {_ != 0} map {case value if value < 10 => "0"+value case value => value.toString},
                        validate(title)
                        ).flatten mkString " - "
}
case class TrackResponse(number:Int,title:String,year:String,genre:String,artist:String,album:String,albumArtist:String,duration:Double) extends Track
case class DiscResponse(info:RawDisc,tracks:Seq[TrackResponse])


