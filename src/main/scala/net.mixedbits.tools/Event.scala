package net.mixedbits.tools

class SimpleEvent{
  private val funcs = new ListBuffer[()=>Any]
  def += (func: =>Any) = {
    val funcRef = func _
    funcs+=funcRef
    funcRef
  }
  
  def apply(){
    for(func <- funcs){
      try{ func() }
      catch{ case _ => () }
    }
  }
}

class Event[T]{
  private val funcs = new ListBuffer[T=>Any]
  def += (func:T=>Any) = {
    funcs+=func
    func
  }
  
  def apply(param:T){
    for(func <- funcs){
      try{ func(param) }
      catch{ case _ => () }
    }
  }
  
  def enabled = funcs.size > 0
}
