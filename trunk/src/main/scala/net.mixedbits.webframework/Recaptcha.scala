package net.mixedbits.webframework

import scala.xml._
import net.mixedbits.tools._

import net.tanesha.recaptcha._

class Recaptcha(val publicKey:String, val privateKey:String,theme:Option[String]){
  def this(publicKey:String,privateKey:String) = this(publicKey,privateKey,None)
  def this(publicKey:String,privateKey:String,theme:String) = this(publicKey,privateKey,Option(theme))
  
  val challengeFieldName = "recaptcha_challenge_field"
  val responseFieldName = "recaptcha_response_field"
  
  private def captchaBuilder = ReCaptchaFactory.newReCaptcha(publicKey,privateKey,false)
  
  private def captchaChecker = {
    val checker = new ReCaptchaImpl
    checker.setPrivateKey(privateKey)
    checker
  }    
  
  def generate():Elem = 
    <div class="captcha">
      {
        (
          for(value <- theme)
            yield
              <script type="text/javascript">var RecaptchaOptions = {"{ theme : '"+value+"' }"};</script>
        ).getOrElse(null)
      }
      <script type="text/javascript" src={"http://api.recaptcha.net/challenge?k="+publicKey}></script>
      <noscript><p>You must have javascript enabled in order to use this form.  Please refer to your web browser help documentation for information on how to do this</p></noscript>
    </div>
  
  
    /*
  def generate():Node =
    Unparsed(captchaBuilder.createRecaptchaHtml(null,null))
    */
  
  def isValid():Boolean = {
    val result = for(
                    challenge <- WebRequest.param(challengeFieldName);
                    response <- WebRequest.param(responseFieldName)
                    ) yield isValid(WebRequest.httpRequest.getRemoteAddr,challenge,response)
                  
    result getOrElse false
  }
  
  def isValid(clientAddress:String,challenge:String,response:String):Boolean = 
    captchaChecker.checkAnswer(clientAddress, challenge, response).isValid
}
