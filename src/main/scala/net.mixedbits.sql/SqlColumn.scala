package net.mixedbits.sql

sealed trait SqlColumnSize{
  def columnType:SqlColumnType
}

sealed trait SqlStringSize extends SqlColumnSize{
  def columnType = SqlStringColumn(this)
}
case class SqlChar(size:Int) extends SqlStringSize //should only allow 1 - 255
case class SqlVarChar(size:Int) extends SqlStringSize //should only allow 1 - 255
case object SqlSmallText extends SqlStringSize
case object SqlMediumText extends SqlStringSize
case object SqlLargeText extends SqlStringSize

sealed trait SqlIntSize extends SqlColumnSize{
  def columnType = SqlIntColumn(this)
}
case object SqlInt8 extends SqlIntSize
case object SqlInt16 extends SqlIntSize
case object SqlInt32 extends SqlIntSize
case object SqlInt64 extends SqlIntSize

sealed trait SqlFloatSize extends SqlColumnSize{
  def columnType = SqlFloatColumn(this)
}
case object SqlFloat32 extends SqlFloatSize
case object SqlFloat64 extends SqlFloatSize

sealed trait SqlBlobSize extends SqlColumnSize{
  def columnType = SqlBlobColumn(this)
}

case object SqlSmallBlob extends SqlBlobSize
case object SqlMediumBlob extends SqlBlobSize
case object SqlLargeBlob extends SqlBlobSize

sealed trait SqlColumnType
case class SqlStringColumn(size:SqlStringSize) extends SqlColumnType
case class SqlIntColumn(size:SqlIntSize) extends SqlColumnType
case class SqlFloatColumn(size:SqlFloatSize) extends SqlColumnType
case class SqlBlobColumn(size:SqlBlobSize) extends SqlColumnType
case object SqlDateTimeColumn extends SqlColumnType
case object SqlBoolColumn extends SqlColumnType
case object SqlAutoIncrementColumn extends SqlColumnType

case class SqlColumn(name:String,columnType:SqlColumnType)

object SqlColumn{
  implicit def fromColumnSize( definition:(Symbol,SqlColumnSize) ):SqlColumn = SqlColumn(definition._1.name,definition._2.columnType)
  implicit def fromColumnType( definition:(Symbol,SqlColumnType) ):SqlColumn = SqlColumn(definition._1.name,definition._2)
}

