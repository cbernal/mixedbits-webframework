package net.mixedbits.webframework

trait WebApplicationLog{
  def logInfo(text:String)
  def logError(text:String)
  def logError(text:String,exeception:Exception)
}

trait WebApplicationConsoleLog extends WebApplicationLog{
  def logInfo(text:String) = println(text)
  def logError(text:String) = Console.err.println(text)
  def logError(text:String,exeception:Exception){
   logError(text)
   exeception.printStackTrace()
  }
}
