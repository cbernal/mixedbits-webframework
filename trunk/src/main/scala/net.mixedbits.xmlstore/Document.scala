package net.mixedbits.xmlstore

case class Document(store:XmlStore,collection:String,id:String,columns:(Symbol,String)*){
  def asString = store.collections(collection).retreiveAsString(id)
  def asDocument = store.collections(collection).retreiveAsDocument(id)
  def as[T] = store.collections(collection).retreiveAsObject(id).asInstanceOf[T]
  
  def apply(symbol:Symbol) = columns.filter(_._1 == symbol).head._2
}
