package net.mixedbits.email

import javax.mail._
import javax.mail.internet._

import net.mixedbits.tools._

trait SmtpMessage{
  def mimeMessage():MimeMessage
}

case class EmailAddress(email:String,name:Option[String]){
  def this(email:String) = this(email,None)
  def this(email:String,name:String) = this(email,Some(name))
  
  def toInternetAddress:InternetAddress = {
    name match {
      case Some(name) => new InternetAddress(email,name)
      case None => new InternetAddress(email)
    }
  }
}

case class FromEmailAddress(sender:EmailAddress,replyTo:Option[EmailAddress]){
  def this(email:String) = this(new EmailAddress(email),None)
  def this(email:String,replyTo:EmailAddress) = this(new EmailAddress(email),Some(replyTo))
  def this(email:String,name:String) = this(new EmailAddress(email,name),None)
  def this(email:String,name:String,replyTo:EmailAddress) = this(new EmailAddress(email,name),Some(replyTo))
  
  def replyToAddress = replyTo getOrElse sender
}

case class PlainTextMessage(to:EmailAddress,from:FromEmailAddress,subject:String,body:String) extends SmtpMessage{
  def mimeMessage():MimeMessage = {
    val mimeMessage = new MimeMessage(null:Session)
    mimeMessage.setRecipients(Message.RecipientType.TO,Array[Address](to.toInternetAddress))
    mimeMessage.setReplyTo(Array(from.replyToAddress.toInternetAddress))
    mimeMessage.setFrom(from.sender.toInternetAddress)
    mimeMessage.setSubject(subject)
    mimeMessage.setContent(body, "text/plain")
    mimeMessage.saveChanges()
    mimeMessage
  }
}

case class SmtpSession(host:Option[String],port:Option[Int],username:Option[String],password:Option[String]){
  
  //deals with all variations of username/password/port/ and hostname
  def connect():Transport = {
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
    
    transport
  }
  
  def send(message:SmtpMessage):PassFail = {
    try{
      val mimeMessage = message.mimeMessage
      
      val transport = connect()
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
  implicit def stringToEmailAddress(s:String) = new EmailAddress(s)
  implicit def stringToFromEmailAddress(s:String) = new FromEmailAddress(s)
}
