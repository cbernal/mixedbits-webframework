package net.mixedbits.webframework.sample

import scala.xml._

import net.mixedbits.webframework._

object SampleApplication extends WebApplication{
  
  val pages = List(
    ""->HomePage,
    "/products"->ProductPage,
    "/products/*"->ProductEntryPage,
    "/blog"->BlogPage,
    "/blog/*"->BlogEntryPage
  )
}

trait DefaultTemplate extends WebPage with WebRequest{
  
  def title = "Sample Web Application"
  def include = List(
    css("/resources/default.css"),
  //  script("/resources/default.js"),
    googleScriptLoader( ("jquery","1",None)/*,("maps","2",None)*/ )
  )
  
  def header =
    <div id="Header">
      <h1>Sample Web Application</h1>
      {navigation}
    </div>
    
  def navigation = 
    <ol>
      <li><a href="/">Home</a></li>
      <li><a href="/products">Products</a></li>
      <li><a href="/blog">Blog</a></li>
    </ol>
    
  def footer = 
    <div id="Footer">
      Copyright {"<123>"} Some Company
    </div>

  def body = 
    <body>
      <div id="PageWrapper">
        {header}
        <div id="BodyWrapper">
        {content.child}
        </div>
        {footer}
      </div>
    </body>
    
  def content:Elem
}

object HomePage extends DefaultTemplate{
  
  def people = List("Ben","Dan")
  
  def content = {
    <page>
      Welcome to some random website, please search for products!
      { for(name <- people) yield <h3>{name}!!!</h3> }
      { WebRequest.webApplication.registeredPages.map{case(path,_)=>{<p>WebPath({path})</p>}} }
    </page>
  }
}

case class Product(name:String,description:String){
  lazy val id = name.toLowerCase.filter(Seq(' ') ++ ('a' to 'z') contains _ ).mkString.replace(' ','-')
  lazy val fulltext = name+"\n"+description
}

object Products{
  def items = List(
    Product("Tasty Cheddar Cheese!","Oh it tastes so good!"),
    Product("Silly American Cheese!","Good but a little bland!"),
    Product("Odd Mozzerella Cheese!","Too bland for me!")
  )  
  
  def find(query:String) = {
    val keywords = query.toLowerCase.split(' ')
    items.filter(item => keywords.forall(item.fulltext.toLowerCase contains _) )
  }
  
  def productById(id:String) = items.filter(_.id == id) match {
    case List(product) => product
    case _ => Product("Not found","No product exists for the specified product id")
  }
  
  def productTemplate(product:Product) = 
    <div class="Product">
      <h1>
        <a href={"/products/"+product.id}>
          {product.name}
        </a>
      </h1>
      {product.description}
    </div>
}


object ProductPage extends DefaultTemplate{
  override def post = ("text/plain","You searched for: "+param("query",""))

  def content = 
    <page>
      <h2>Search for products</h2>
      <form method="get">
        <input name="show" type="hidden" value="all" />
        <input type="submit" value="Show all products..." />
      </form>
      <hr />
      <form method="post">
        <label for="query">Search for products</label>
        <input id="query" name="query" type="text" value={param("query","bland cheese")} />
        <input type="submit" value="Search..." />
      </form>
      <hr />
      {
        if(param("show","") == "all"){
          for(product <- Products.items)
            yield Products.productTemplate(product)
        }
        else if(param("query","")!=""){
          <div>
          <strong>You searched for: {param("query","")}</strong>
          { Products.find(param("query","")).map(Products.productTemplate(_)) }
          </div>
        }
        else{          
          <h2>Please search for a product</h2>
        }
      }
    </page>

}

object ProductEntryPage extends DefaultTemplate{
  def content = 
    <page>
    { Products.productTemplate(Products.productById(webpath.wildcards.first)) }
    </page>
}

case class BlogEntry(title:String,body:Elem){
  lazy val id = title.toLowerCase.filter(Seq(' ') ++ ('a' to 'z') contains _ ).mkString.replace(' ','-')
}
object Blog{
  def noEntryFound = "No matching blog entry found!"
  def entryById(id:String):Option[BlogEntry] = entries.filter(_.id == id) match {
    case List(entry) => Some(entry)
    case _ => None
  }
  def entryTemplate(entry:Option[BlogEntry]):Elem = entry match{
    case Some(blogEntry) => entryTemplate(blogEntry)
    case None => <h1>{noEntryFound}</h1>
  }
  def entryTemplate(entry:BlogEntry):Elem = 
    <div class="BlogEntry">
      <h1>
        <a href={"/blog/"+entry.id}>
          {entry.title}
        </a>
      </h1>
      {entry.body.child}
    </div>

  val entries = List(
    BlogEntry(
      "What in the world?",
      <body>
        I have no idea what I'm talking about, but I saw <a href="http://www.google.com">this</a> and thought it was interesting
      </body>
    ),
    BlogEntry(
      "Who said that?",
      <body>
        <p>Umm... yeah</p>
        <p><b>No</b></p>
      </body>
    ),
    BlogEntry(
      "Who said that. It definitely wasn't me.",
      <body>
        <p>Umm... yeah</p>
        <p><b>No</b></p>
      </body>
    )
  )

}

object BlogPage extends DefaultTemplate{
  override def title = super.title + " :: Blog"
  def content = 
    <page>
    { for(entry <- Blog.entries) yield Blog.entryTemplate(entry) }
    </page>
}

object BlogEntryPage extends DefaultTemplate{
  override def title = super.title + " :: Blog :: "+currentBlogEntry.map(_.title).getOrElse(Blog.noEntryFound)
  def currentBlogEntry = Blog.entryById(webpath.wildcards.first) 
  def content = 
    <page>
    { Blog.entryTemplate(currentBlogEntry) }
    </page>
}
