package net.mixedbits.webframework

import scala.xml._

import net.tanesha.recaptcha._

class Recaptcha(val publicKey:String, val privateKey:String){
  val challengeFieldName = "recaptcha_challenge_field"
  val responseFieldName = "recaptcha_response_field"
  
  private def captchaBuilder = ReCaptchaFactory.newReCaptcha(publicKey,privateKey,false)
  
  private def captchaChecker = {
    val checker = new ReCaptchaImpl
    checker.setPrivateKey(privateKey)
    checker
  }
  
  def generate():Elem =
    <script type="text/javascript" src={"http://api.recaptcha.net/challenge?k="+publicKey}></script>
  
  
    /*
  def generate():Node =
    Unparsed(captchaBuilder.createRecaptchaHtml(null,null))
    */
  
  def isValid():Boolean = {
    val request = net.mixedbits.webframework.WebRequest.httpRequest
    isValid(request.getRemoteAddr,request.getParameter(challengeFieldName),request.getParameter(responseFieldName))
  }
  
  def isValid(clientAddress:String,challenge:String,response:String):Boolean = 
    captchaChecker.checkAnswer(clientAddress, challenge, response).isValid
}
