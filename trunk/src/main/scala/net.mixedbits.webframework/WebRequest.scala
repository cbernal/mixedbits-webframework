package net.mixedbits.webframework

object WebRequest{
  import scala.util.DynamicVariable
  import javax.servlet.{ServletContext,http}
  import javax.servlet.http.{HttpServletRequest,HttpServletResponse}
  import net.mixedbits.tools._
  
  val requestContext = new DynamicVariable[(WebApplication,ServletContext,HttpServletRequest,HttpServletResponse,WebPathMatch)](null)
  
  def param(name:String) = 
    Objects.makeOption(httpRequest.getParameter(name))

  def webApplication = requestContext.value._1
  def servletContext = requestContext.value._2
  def httpRequest = requestContext.value._3
  def httpResponse = requestContext.value._4
  def webpath = requestContext.value._5
}


trait WebRequest{
 
  def param = WebRequest.param(_)
  def param(name:String,default:String):String = param(name).getOrElse(default)
  def webpath = WebRequest.webpath
  
  def params = WebRequest.httpRequest.getParameterMap.asInstanceOf[java.util.Map[String,Array[String]]]
  
}

/*
trait DatabaseConnection{
  import java.sql._
  import javax.naming.InitialContext
  import javax.sql.DataSource
  
  def databaseConnectionName:String
  private lazy val databaseDataSource = 
    try{
      new InitialContext().lookup(databaseConnectionName).asInstanceOf[DataSource]
    }
    catch{
      case e => {
         println("Unable to load datasource from jndi")
        
      //  val datasource = new com.mysql.jdbc.jdbc2.optional.MysqlConnectionPoolDataSource
      //  datasource.setURL("jdbc:mysql://localhost:3306/WebApp?useUnicode=true&characterEncoding=UTF-8")
      //  datasource.setUser("webapp")
      //  datasource.setPassword("webapp")
      //  datasource
        
      
      //  println("creating server")
      //  org.h2.tools.Server.main("-web","-webAllowOthers")
        println("creating datasource")
        val datasource = new org.h2.jdbcx.JdbcDataSource()
        datasource.setURL("jdbc:h2:~/WebApp")
        datasource.setUser("webapp")
        datasource.setPassword("webapp")
        datasource
      }
    }

  def databaseConnection = {
		val connection = databaseDataSource.getConnection
		connection.setReadOnly(false)
		connection
  }
  
  def readonlyDatabaseConnection = {
		val connection = databaseDataSource.getConnection
		connection.setReadOnly(true)
		connection
  }
  
  
  def execute[T](sql:String)(body:ResultSet=>T) = {
    val results = new scala.collection.mutable.ListBuffer[T]

    using(readonlyDatabaseConnection){
      connection =>
      using(connection.createStatement){
        statement =>
        using(statement.executeQuery(sql)){
          resultSet =>
    
          while(resultSet.next)
            results += body(resultSet)
    
        }
      }
    }
    
    results.toList
  }
  
  def using[A <: {def close():Unit},B](param:A)(f:A=>B):B = 
    try{
      f(param)
    }
    finally{
      param.close
    }
  
}
*/

