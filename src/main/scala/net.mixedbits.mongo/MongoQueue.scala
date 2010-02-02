package net.mixedbits.mongo

import net.mixedbits.tools._
import net.mixedbits.tools.BlockStatements._
import java.util.Date

import org.scala_tools.time._
import org.scala_tools.time.Imports._

abstract class MongoQueue[T <: JsDocument](collection:MongoBaseCollection[T]) extends JsObjectProperty{
  queue =>
  
  def this(collection:MongoBaseCollection[T],name:String) = {this(collection);propertyName(name)}
  def this(collection:MongoBaseCollection[T],parent:JsProperty[_]) = {this(collection);propertyName(parent.propertyName+"."+Objects.objectPath(this).last)}
  def this(collection:MongoBaseCollection[T],parent:JsProperty[_],name:String) = {this(collection);propertyName(parent.propertyName+"."+name)}
  
  sealed abstract class MongoQueueResult(val updates:Option[MongoUpdate],val processTime:Option[DateTime])
  case class UpdateItem(newUpdates:MongoUpdate) extends MongoQueueResult(Some(newUpdates),None)
  case class UpdateAndEnqueueItem(newUpdates:MongoUpdate,newProcessTime:DateTime) extends MongoQueueResult(Some(newUpdates),Some(newProcessTime))
  case class EnqueueItem(newProcessTime:DateTime) extends MongoQueueResult(None,Some(newProcessTime))
  case object RemoveItem extends MongoQueueResult(None,None)
  case object ProcessingComplete extends MongoQueueResult(None,None)
  case object ItemFailed extends MongoQueueResult(None,None)
  
  private sealed abstract class NextItemResult
  private case class ItemClaimed(item:T) extends NextItemResult
  private case object ItemMissed extends NextItemResult
  private case object NoItemFound extends NextItemResult
  
  //events
  protected val onStart = new SimpleEvent
  protected val onStop = new SimpleEvent
  protected val onIdle = new SimpleEvent
  protected val onItemMissed = new SimpleEvent
  protected val onBeforeItem = new Event[T]
  protected val onAfterItem = new Event[T]
  protected val onClaimTimeout = new Event[T]
  protected val onItemFailed = new Event[(T,Throwable)]
  protected val onUpdateFailed = new Event[(T,Throwable)]
  
  def start(){
    _enabled = true
    //start background thread if necessary
    ensureThreadIsRunning()
  }
  
  def stop(){
    _enabled = false
    //background thread should die once the current item is finished...
  }
  
  def join(){
    var threadToJoin:Thread = null
    threadLock synchronized {
      threadToJoin = currentThread
    }
    
    if(threadToJoin != null)
      threadToJoin.join
  }
  
  def totalEligible() = 
    collection.find(ClaimedBy == null and ScheduledTime <= new Date()).count
  
  private val threadLock = new AnyRef
  
  def enabled = _enabled
  private var _enabled = false
  private var currentThread:Thread = null
  
  private def ensureThreadIsRunning(){
    threadLock synchronized {
      if( currentThread == null ){
        currentThread = createThread()
        currentThread.start()
      }
    }
  }
  
  private def createThread() = daemonThread{
    onStart()
    
    //do code that handles reading from the queue here...
    while(enabled){
      nextItem match {
        case ItemClaimed(item) =>
          onBeforeItem(item)
          processItem(item)
          onAfterItem(item)
          if(enabled)
            Thread.sleep(queuePauseDuration.millis)
        case NoItemFound =>
          if(enabled){
            onIdle()
            Thread.sleep(queueIdleDuration.millis)
          }
        case ItemMissed =>
          onItemMissed()
      }
    }
    
    threadLock synchronized {
      currentThread = null
    }
    
    onStop()
  }
  
  private def nextItem():NextItemResult = {
    def findAndClaim(criteria:MongoConstraint) = 
      //find items and ensure that they were properly claimed here
      collection.findOne(criteria) match {
        case Some(item) =>
          //ensure that we have all appropriate fields, then attempt to use them to update the item database, thereby "claiming" the item
          val claimedItem = for(
                              itemId <- item(JsDocument.Id);
                              uniqueId <- item(queue.UniqueId);
                              updated = collection.find(JsDocument.Id == itemId and queue.UniqueId == uniqueId)
                                              .updateFirst(
                                                  queue.UniqueId <~ MongoTools.generateId() and
                                                  ClaimedBy <~ serverName and
                                                  LastStarted <~ new Date() and
                                                  LastFinished <~ None
                                                  )
                              if updated
                              ) yield item
          claimedItem match {
            case Some(item) => ItemClaimed(item)
            case None => ItemMissed
          }
        case None =>
          NoItemFound
      }
    
    //claimed items that have exceeded the claim timeout
    val timeoutClaimResult = findAndClaim(ClaimedBy != null and LastFinished == null and LastStarted <= (DateTime.now - queueClaimTimeout).toDate)
    
    timeoutClaimResult match {
      
      //if there weren't any timeout items to run, get a normal item
      case ItemMissed | NoItemFound =>
        //unclaimed items that area ready to run
        findAndClaim(ClaimedBy == null and ScheduledTime <= new Date())
        
      //we claimed a timeout item, send notification, and then process the item
      case ItemClaimed(item) =>
        
        onClaimTimeout(item)
      
        timeoutClaimResult
    }
  }
  
  private def processItem(item:T){
    try{
      val result = onItemSelected(item)
      processItemResult(item,result)
    }
    catch{
      case e =>
        onItemFailed(item,e)
        processItemResult(item,ItemFailed)

        println("error processing item")
        e.printStackTrace()
    }
  }
  
  private def processItemResult(item:T,result:MongoQueueResult):Unit = 
    processItemResult(item,result,0)
  
  private def processItemResult(item:T,result:MongoQueueResult,attempt:Int){
    def updateItem(updates:MongoUpdate) = 
      for(itemId <- item(JsDocument.Id))
        yield collection.find(JsDocument.Id == itemId and ClaimedBy == serverName)
          .updateFirst(updates)
    
    try{

      result match {
        case ItemFailed =>
          // re-enqueue for immediate processing and increment retry counter
          updateItem(enqueue() and RetryCount + 1) match {
            case Some(true) => ()
            case Some(false) => error("update failure: couldn't find item to update")
            case None => error("update failure: couldn't find id of item to update")
          }
          
        case RemoveItem => 
          //code to remove item...
          collection.remove(item)
          
        case results =>
          //all other cases
          val newValues = results.updates
          val newProcessTime = results.processTime.map(ScheduledTime <~ _.toDate and queue.UniqueId <~ MongoTools.generateId)
          
          //make sure we have an id, make sure we claimed the item, and then mark it as processed and apply the users requested changes
          updateItem(ScheduledTime <~ None and ClaimedBy <~ None and LastFinished <~ new Date() and TimesProcessed + 1 and newValues and newProcessTime) match {
            case Some(true) => ()
            case Some(false) => error("update failure: couldn't find item to update")
            case None => error("update failure: couldn't find id of item to update")
          }

      }
    }
    catch{
      case e =>
        // retry a couple of times, then send notification
        if(attempt >= maxUpdateAttempts)
          onUpdateFailed(item,e)
        else
          processItemResult(item,result,attempt+1)
      
        println("error updating item")
        e.printStackTrace()
    }
  }
  
  def enqueue(item:T):Boolean =
    enqueue(item, DateTime.now)
  def enqueue(item:T,processTime:DateTime):Boolean = {
    val result = for(id <- item(JsDocument.Id))
                    yield collection
                            .find(JsDocument.Id == id)
                            .updateFirst(enqueue(processTime))
    result getOrElse false
  }
  
  def enqueue():MongoUpdate = 
    enqueue(DateTime.now)
  def enqueue(processTime:DateTime):MongoUpdate = 
    ScheduledTime <~ processTime.toDate and queue.UniqueId <~ MongoTools.generateId and ClaimedBy <~ None
  
  def onItemSelected(item:T):MongoQueueResult
  
  //how long to wait after running out of items in millis
  def queueIdleDuration:Duration
  
  //how long to wait after completing an item before continuing to the next, in millis
  def queuePauseDuration:Duration
  
  //how long to wait before stealing a claimed item from another server
  def queueClaimTimeout:Duration
  
  def maxUpdateAttempts = 5
  
  //this will be used to identify who claimed the item
  val serverName:String = Network.hostname
  
  object ScheduledTime extends JsDateProperty(queue)
  object UniqueId extends JsStringProperty(queue)
  object ClaimedBy extends JsStringProperty(queue)
  object LastFinished extends JsDateProperty(queue)
  object LastStarted extends JsDateProperty(queue)
  object TimesProcessed extends JsIntProperty(queue)
  object RetryCount extends JsIntProperty(queue)
}