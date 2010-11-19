package net.mixedbits.tools

object ConsoleProgress{
  
  def apply(label:String,width:Int,labelColor:Option[String] = None, progressColor:Option[String] = None, percentColor:Option[String] = None):Double => Unit = {
    var previousLineWidth = 0
    var complete = false
    
    {progress:Double =>
      if(!complete){
        //clear the old line
        print("\b" * previousLineWidth)
        
        //print the new line
        val barWidth = (width * progress).toInt
        val barRemainder = width - barWidth
        
        val labelOutput = label+": "
        val progressBar = "["+("=" * barWidth)+(" " * barRemainder)+"] "
        val percent = (math.round(progress * 1000.0)/10.0)+"%"
        
        labelColor foreach print
        print(labelOutput)
        if(labelColor.isDefined) print(Console.RESET)
        
        progressColor foreach print
        print(progressBar)
        if(progressColor.isDefined) print(Console.RESET)
        
        percentColor foreach print
        print(percent)
        if(percentColor.isDefined) print(Console.RESET)
                
        previousLineWidth = labelOutput.size + progressBar.size + percent.size
        
        //deal with 100% progress
        if(progress >= 1.0){
          complete = true
          println()
        }
      }
    }
  }
}
