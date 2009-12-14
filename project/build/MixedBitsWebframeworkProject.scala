import sbt._

class MixedBitsWebframeworkProject(info: ProjectInfo) extends DefaultWebProject(info){
  currentProject =>
  
  val jetty6 = "org.mortbay.jetty" % "jetty" % "6.1.14" % "test->default"  // jetty is only need for testing
  val jasper = "org.apache.tomcat" % "jasper" % "6.0.18"
  
  lazy val refresh = task{jettyRun.run;None} dependsOn(clean)
}
