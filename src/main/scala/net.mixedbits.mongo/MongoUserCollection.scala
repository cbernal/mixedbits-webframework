package net.mixedbits.mongo


import net.mixedbits.json._
import net.mixedbits.tools._
import net.mixedbits.tools.Objects._

trait MongoUserCollection{
  self:MongoCollection =>
  
  def usernameProperty:JsStringProperty
  def passwordProperty:JsStringProperty
  
  //maybe we need to immediately store this in the database and verify that the username didn't exist?
  def createUser(username:String,password:String):JsDocument =
    new JsDocument()
      .update(usernameProperty,username)
      .update(passwordProperty,Passwords.hash(password))
      
  //def changePassword(username:String,newPassword:String){}
  //def changePassword(username:String,oldPassword:String,newPassword:String){}
  
  //def changeUsername(oldUsername:String,newUsername:String){}
  //def changeUsername(documentId:String,newUsername:String){}

  def findByUsername(username:String) = 
    findOne(usernameProperty == username)

  def attemptLogin(username:String,password:String):Option[JsDocument] = 
    for(
      doc <- findByUsername(username);
      hashedPassword <- doc(passwordProperty);
      if Passwords.areEqual(password,hashedPassword)
      ) yield doc

  //maybe we should try to create a unique index?
  //depends on whether or not we're sharding, maybe application logic on create/change username would be better...
  index(usernameProperty)
}
