package net.mixedbits.xmlstore

object UniqueId{
  
  def apply():String = {
    
    val bb = java.nio.ByteBuffer.wrap( Array.ofDim[Byte](12) ).putInt( _nextInc.getAndIncrement() ).putInt( _genmachine ).putInt( _curtime() )
    val b = bb.array().reverse
    
    val buf = new StringBuilder(24)
    
    for (value <- b){
      val s = Integer.toHexString( value & 0xFF )
      if ( s.length() == 1 )
        buf.append( "0" )
      buf.append( s )
    }

    return buf.toString()
  }
  
  private def _flip( x:Int ) = {
    var z = 0;
    z |= ( ( x << 24 ) & 0xFF000000 )
    z |= ( ( x << 8 )  & 0x00FF0000 )
    z |= ( ( x >> 8 )  & 0x0000FF00 )
    z |= ( ( x >> 24 ) & 0x000000FF )
    z
  }
  
  private def _curtime() =  _flip( (System.currentTimeMillis()/1000).asInstanceOf[Int] )
  private val _nextInc = new java.util.concurrent.atomic.AtomicInteger( (new java.util.Random()).nextInt() )
  private val _genmachine = {
    val machinePiece = {
      val sb = new StringBuilder()
      val e = java.net.NetworkInterface.getNetworkInterfaces()
      while ( e.hasMoreElements() ){
        sb.append( e.nextElement().toString() )
      }
      
      sb.toString().hashCode() << 16
      }
    
    val processPiece = java.lang.management.ManagementFactory.getRuntimeMXBean().getName().hashCode() & 0xFFFF
    machinePiece | processPiece;
    }
}
