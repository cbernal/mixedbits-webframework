package net.mixedbits.email

import javax.mail._
import javax.mail.internet._

import net.mixedbits.tools._

object Email{
  def send(server:String,to:String,from:String,subject:String,body:String):PassFail = {
    try{
      val properties = new java.util.Properties
      properties.put("mail.smtp.host",server)
      
      val session = Session.getInstance(properties, null)
      val message = new MimeMessage(session)
      message.setRecipients(Message.RecipientType.TO,Array[Address](new InternetAddress(to)))
      message.setReplyTo(Array(new InternetAddress(from)))
      message.setFrom(new InternetAddress(from))
      message.setSubject(subject)
      message.setContent(body, "text/plain")
      Transport.send(message)
      
      Success
    }
    catch{
      case e => Failure(e)
    }
  }
  
}
