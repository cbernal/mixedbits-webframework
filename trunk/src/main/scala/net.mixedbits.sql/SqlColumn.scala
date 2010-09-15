package net.mixedbits.sql

sealed trait SqlStringSize
case class SqlChar(size:Int) extends SqlStringSize //should only allow 1 - 255
case class SqlVarChar(size:Int) extends SqlStringSize //should only allow 1 - 255
case object SqlSmallText extends SqlStringSize
case object SqlMediumText extends SqlStringSize
case object SqlLargeText extends SqlStringSize

sealed trait SqlIntSize
case object SqlInt8 extends SqlIntSize
case object SqlInt16 extends SqlIntSize
case object SqlInt32 extends SqlIntSize
case object SqlInt64 extends SqlIntSize

sealed trait SqlFloatSize
case object SqlFloat32 extends SqlFloatSize
case object SqlFloat64 extends SqlFloatSize

sealed trait SqlColumnType
case class SqlStringColumn(size:SqlStringSize) extends SqlColumnType
case class SqlIntColumn(size:SqlIntSize) extends SqlColumnType
case class SqlFloatColumn(size:SqlFloatSize) extends SqlColumnType
case object SqlDateTimeColumn extends SqlColumnType
case object SqlBoolColumn extends SqlColumnType
case object SqlAutoIncrementColumn extends SqlColumnType

case class SqlColumn(name:String,columnType:SqlColumnType)

