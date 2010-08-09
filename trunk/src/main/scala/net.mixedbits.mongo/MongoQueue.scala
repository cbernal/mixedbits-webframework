package net.mixedbits.mongo

import net.mixedbits.json._
import net.mixedbits.tools._
import java.util.Date

import org.scala_tools.time._
import org.scala_tools.time.Imports._

abstract class MongoQueue[T <: JsDocument](collectionReference: => MongoBaseCollection[T]) extends JsObjectProperty{
  queue =>
  
  lazy val collection = collectionReference
  
  def this(collection:MongoBaseCollection[T],name:String) = {this(collection);propertyName(name)}
  def this(collection:MongoBaseCollection[T],parent:JsProperty[_]) = {this(collection);propertyName(parent.propertyName+"."+Objects.objectPath(this).last)}
  def this(collection:MongoBaseCollection[T],parent:JsProperty[_],name:String) = {this(collection);propertyName(parent.propertyName+"."+name)}
  
  sealed abstract class MongoQueueResult(val updates:Option[JsUpdate],val processTime:Option[DateTime])
  case class UpdateItem(newUpdates:JsUpdate) extends MongoQueueResult(Some(newUpdates),None)
  case class UpdateAndEnqueueItem(newUpdates:JsUpdate,newProcessTime:DateTime) extends MongoQueueResult(Some(newUpdates),Some(newProcessTime))
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
  protected val onSpeedUpdate = new Event[(Int,Duration)]
  
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
  
  def totalEligible(time:DateTime):Long = 
    collection.find(ScheduledTime <= time.toDate and ClaimedBy == null).count
    
  def totalEligible():Long = 
    totalEligible(DateTime.now)
  
  private val threadLock = new AnyRef
  
  def enabled = _enabled
  private var _enabled = false
  private var currentThread:Thread = null
  
  
  def totalProcessed = _totalProcessed
  private var _totalProcessed = 0
  private var _currentProcessed = 0
  private var _statsStartTime = 0L
  
  private def incrementStats(){
    _totalProcessed+=1
    _currentProcessed+=1
    
    //check to see if we should send stats
    sendSpeedStats()
  }
  
  private def prepareSpeedStats(){
    _currentProcessed = 0
    _statsStartTime = System.currentTimeMillis
  }
  
  private def sendSpeedStats(){sendSpeedStats(false)}
  private def sendSpeedStats(force:Boolean){
    if(!onSpeedUpdate.enabled)
      return
    
    val currentTime = System.currentTimeMillis
    val elapsed = currentTime - _statsStartTime
    if(force || elapsed > _statsSpeedDurationInMillis){
      onSpeedUpdate( (_currentProcessed,new Duration(elapsed)) )
      prepareSpeedStats()
    }
  }
  
  private def ensureThreadIsRunning(){
    threadLock synchronized {
      if( currentThread == null ){
        currentThread = createThread()
        currentThread.start()
      }
    }
  }
  
  lazy val queueName = Objects.simpleClassName(this)
  
  private def createThread() = daemonThread{
    onStart()
    
    //always prepare to capture speed stats when starting a new thread
    prepareSpeedStats()
    
    val pauseMillis = queuePauseDuration.millis
    val idleMillis = queueIdleDuration.millis
    
    //do code that handles reading from the queue here...
    while(enabled){
      nextItem match {
        case ItemClaimed(item) =>
          onBeforeItem(item)
          processItem(item)
          onAfterItem(item)
          
          //always increments stats after processing an item
          incrementStats()
          
          if(enabled && pauseMillis > 0)
            Thread.sleep(pauseMillis)
          
        case NoItemFound =>
          //always send speed stats right before going idle...
          sendSpeedStats(true)
          
          if(enabled){
            onIdle()
            if(idleMillis > 0)
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
  } |>> {_.setUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler{
    def uncaughtException( t:Thread, e:Throwable){
      println("queue died:"+queueName)
      e.printStackTrace()
    }
  })}
   
  
  private def nextItem():NextItemResult = {
    try {

      def findAndClaim(criteria:JsConstraint) = 
        //find items and ensure that they were properly claimed here
        collection.findOne(criteria) match {
          case Some(item) =>
            
            val markAsClaimed = queue.UniqueId <~ JsTools.generateId() and
                              ClaimedBy <~ serverName and
                              LastStarted <~ new Date() and
                              LastFinished <~ None
  
            //ensure that we have all appropriate fields, then attempt to use them to update the item database, thereby "claiming" the item
            val claimedItem = for(
                                itemId <- item(JsDocument.Id);
                                uniqueId <- item(queue.UniqueId);
                                updated = collection
                                                .find(JsDocument.Id == itemId and queue.UniqueId == uniqueId)
                                                .updateFirst(markAsClaimed)
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
      val timeoutClaimResult = findAndClaim(LastStarted <= (DateTime.now - queueClaimTimeout).toDate and LastFinished == null and ClaimedBy != null)
      
      timeoutClaimResult match {
        
        //if there weren't any timeout items to run, get a normal item
        case ItemMissed | NoItemFound =>
          //unclaimed items that area ready to run
          findAndClaim(ScheduledTime <= new Date() and ClaimedBy == null)
          
        //we claimed a timeout item, send notification, and then process the item
        case ItemClaimed(item) =>
          
          onClaimTimeout(item)
        
          timeoutClaimResult
      }
    
    } catch {
      case e => NoItemFound
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
    def updateItem(updates:JsUpdate) = 
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
          val newProcessTime = results.processTime.map(ScheduledTime <~ _.toDate and queue.UniqueId <~ JsTools.generateId)
          
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
  
  def enqueue():JsUpdate = 
    enqueue(DateTime.now)
  def enqueue(processTime:DateTime):JsUpdate = 
    ScheduledTime <~ processTime.toDate and queue.UniqueId <~ JsTools.generateId and ClaimedBy <~ None
  
  def isEnqueued(item:T):Boolean = 
    (item contains queue.ScheduledTime) && (item contains queue.UniqueId)
  
  def onItemSelected(item:T):MongoQueueResult
  
  //how long to wait after running out of items in millis
  def queueIdleDuration:Duration
  
  //how long to wait after completing an item before continuing to the next, in millis
  def queuePauseDuration:Duration
  
  //how long to wait before stealing a claimed item from another server
  def queueClaimTimeout:Duration
  
  //how many times to attempt applying item updates before giving up
  def maxUpdateAttempts = 5
  
  
  def indexForScheduledItems() = collection.index(queueName+"-scheduled-time",queue.ScheduledTime -> SortDirection.Ascending)
  def indexForExpiredItems() = collection.index(queueName+"-laststarted-lastfinished",queue.LastStarted -> SortDirection.Ascending,queue.LastFinished -> SortDirection.Ascending)
  
  //def statsSpeedDuration:Duration
  //
  //private lazy val _statsSpeedDurationInMillis = statsSpeedDuration.millis
  private val _statsSpeedDurationInMillis = 1.minute.millis
  
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
