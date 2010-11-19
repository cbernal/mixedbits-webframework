package net.mixedbits

package object tools
  extends NumbersImports
  with StringsImports
  with SequencesImports
  with DatesImports
  with FilesImports
  with ObjectsImports
  with BlockStatementsImports
  with CloseableResourceImports
  with XmlImports{
    
  type ListBuffer[A] = scala.collection.mutable.ListBuffer[A]
  type ArrayBuffer[A] = scala.collection.mutable.ArrayBuffer[A] 
}
