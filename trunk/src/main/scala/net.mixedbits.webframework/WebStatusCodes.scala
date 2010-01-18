package net.mixedbits.webframework

sealed abstract case class HttpRedirect(code:Int)
object HttpRedirect{
  case object Permanent extends HttpRedirect(301)
  case object Found extends HttpRedirect(302)
  case object SeeOther extends HttpRedirect(303)
  case object Temporary extends HttpRedirect(307)
}
