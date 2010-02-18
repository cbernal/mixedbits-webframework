import sbt._

class MixedBitsWebframeworkProject(info: ProjectInfo) extends DefaultWebProject(info){
  currentProject =>


  /******************
  | dependencies    |
  ******************/
  
  //web server  
  val jetty6 = "org.mortbay.jetty" % "jetty" % "6.1.14" % "test->default"  // jetty is only need for testing
  val jasper = "org.apache.tomcat" % "jasper" % "6.0.18"
  
  //multipart file uploading
  val commons_fileupload = "commons-fileupload" % "commons-fileupload" % "1.2"
  
  //smtp library
  val java_net_repo = "java.net" at "http://download.java.net/maven/2/"
  val java_mail = "javax.mail" % "mail" % "1.4.2"

  //joda time wrapper
  val scala_time = "org.scala-tools" % "time" % "2.7.4-0.1"
  
  //recaptcha library
  val recaptcha_repo = "taneshanet" at "http://tanesha.net/maven2"
  val recaptcha4j = "net.tanesha.recaptcha4j" % "recaptcha4j" % "0.0.7"
  

  /******************
  | building        |
  ******************/
  
  lazy val refresh = task{jettyRun.run;None} dependsOn(clean)
}
