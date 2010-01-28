package net.mixedbits.email

import javax.mail._
import javax.mail.internet._

import net.mixedbits.tools._

case class PlainTextMessage(to:String,from:String,subject:String,body:String)
case class SmtpSession(host:Option[String],port:Option[Int],username:Option[String],password:Option[String]){
  /*
  def session() = {
    
    mail.smtp.host
    mail.smtp.port
    
    mail.smtp.user
    mail.smtp.auth
  }
  */
  def send(message:PlainTextMessage):PassFail = {
    try{
      
      val properties = new java.util.Properties
      for(h <- host)
        properties.put("mail.smtp.host",h)
      
      val session = Session.getInstance(properties, null)
      val transport = session.getTransport("smtp")
      
      for(h <- host; p <- port; user <- username; pass <- password; if !transport.isConnected)
        transport.connect(h,p,user,pass)
      for(h <- host; user <- username; pass <- password; if !transport.isConnected)
        transport.connect(h,user,pass)
      for(user <- username; pass <- password; if !transport.isConnected)
        transport.connect(user,pass)
      
      if(!transport.isConnected)
        transport.connect()
      
      
      val mimeMessage = new MimeMessage(session)
      mimeMessage.setRecipients(Message.RecipientType.TO,Array[Address](new InternetAddress(message.to)))
      mimeMessage.setReplyTo(Array(new InternetAddress(message.from)))
      mimeMessage.setFrom(new InternetAddress(message.from))
      mimeMessage.setSubject(message.subject)
      mimeMessage.setContent(message.body, "text/plain")
      
      mimeMessage.saveChanges()
      transport.sendMessage(mimeMessage,mimeMessage.getAllRecipients)
      transport.close
      
      Success
    }
    catch{
      case e => Failure(e)
    }
  }
}

object Email{
  
  /*
  def send(server:String,to:String,from:String,subject:String,body:String):PassFail = {
    try{

      
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
  */
}
