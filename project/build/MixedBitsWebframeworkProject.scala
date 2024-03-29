import sbt._

class MixedBitsWebframeworkProject(info: ProjectInfo) extends DefaultProject(info){
  currentProject =>


  /******************
  | dependencies    |
  ******************/

  override def repositories = Set(
    "paranamer" at "http://repository.codehaus.org",
    "taneshanet" at "http://tanesha.net/maven2",
    "java.net" at "http://download.java.net/maven/2/",
    ScalaToolsSnapshots
  )
 
  override def libraryDependencies = Set(
    "org.eclipse.jetty" % "jetty-server" % "7.0.1.v20091125" % "test->default",
    "org.eclipse.jetty" % "jetty-webapp" % "7.0.1.v20091125" % "test->default",
    "org.apache.tomcat" % "jasper" % "6.0.18" % "provided",
    "commons-fileupload" % "commons-fileupload" % "1.2",
    "javax.mail" % "mail" % "1.4.2" % "provided",
    "joda-time" % "joda-time" % "1.6",
    "net.tanesha.recaptcha4j" % "recaptcha4j" % "0.0.7",
    "com.thoughtworks.xstream" % "xstream" % "1.3.1",
    "com.h2database" % "h2" % "1.2.140",
    "org.codehaus.jettison" % "jettison" % "1.2",
    "org.mongodb" % "mongo-java-driver" % "2.3",
    "net.sf.opencsv" % "opencsv" % "2.0"
  )
  
  
  /******************
  | misc            |
  ******************/
  
  override def consoleInit =
    """
    import java.io._
    import net.mixedbits.media._
    import net.mixedbits.json._
    import net.mixedbits.mongo._
    import net.mixedbits.tools._
    import net.mixedbits.webframework._
    import net.mixedbits.xmlstore._
    import net.mixedbits.xmlstore.schema._
    import net.mixedbits.sql._
    import org.scala_tools.time.Imports._
    """
  

  /******************
  | building        |
  ******************/
  
  override def compileOptions = super.compileOptions ++ (Seq("-unchecked","-g:vars") map {CompileOption(_)})

}
