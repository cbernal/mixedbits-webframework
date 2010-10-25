package net.mixedbits.xmlstore

case class Document(store:XmlStore,collection:String,id:String,columns:(Symbol,String)*){
  lazy val collectionReference = store.collections(collection)
  def rawDocument = column('document) getOrElse collectionReference.retreiveAsString(id)
  
  def asString = rawDocument
  def asDocument = collectionReference.loadAsDocument(rawDocument)
  def asElem = collectionReference.loadAsElem(rawDocument)
  def as[T] = collectionReference.loadAsObject(rawDocument).asInstanceOf[T]
  
  def apply(symbol:Symbol) = columns.filter(_._1 == symbol).head._2
  def column(symbol:Symbol) = columns.filter(_._1 == symbol).headOption.map(_._2)
}
