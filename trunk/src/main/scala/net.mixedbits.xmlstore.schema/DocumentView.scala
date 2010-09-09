package net.mixedbits.xmlstore.schema


case class DocumentView(name:String,select:String,columns:Seq[DocumentColumn])
