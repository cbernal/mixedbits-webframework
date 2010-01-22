package net.mixedbits.tools

sealed abstract class PassFail

case object Success extends PassFail
case class Failure(exception:Throwable) extends PassFail
