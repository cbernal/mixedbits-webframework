package net.mixedbits.media

object Time{

  def parseDuration(value:String):Double = {
    val multipliers = Array(60,60*60,24*60*60)
    val parts = value split ":"
    val remainder = parts.reverse drop 1
    var duration = (parts last) toDouble
    
    for((x,multiplier) <- remainder zip multipliers)
      duration += x.toDouble * multiplier
    
    duration
  }
}
