import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef

// Diagram of Actors and messages
//
//                                       |
//                                       |
//                                 StartIndexing
//                                       |
//                                       V

/////////////  <-- IndexRequest --   ////////////  <----- Query ----- //////////////
// Fetcher //                        // Master //                     // Prompter //
///////////// -- Option[RawPage] --> ////////////  -- QueryResult --> //////////////


//////////////////////////////////
// Case classes for messages

case class StartIndexing(urls: Seq[String])

case class IndexRequest(url: String)
case class RawPage(url: String, html: String) {
	// Override toString so printed messages do not
	//	 dump the full html
	override def toString() = url
}

case class Query(terms: Seq[String])
case class QueryResult(fractionContaining: Double, numTotalPages: Int)

//////////////////////////////////
// Actors

class Fetcher extends Actor {
	def receive = {
		case IndexRequest(url) => {
			try {
				val src = scala.io.Source.fromURL(url).getLines.mkString("\n")
				sender ! Some(RawPage(url, src))
			} catch {
				case e: java.io.IOException => sender ! None
				//typically a binary file
			}
		}
	}
}

// Prompter asks the user to enter queries,
//	 then sends them along to the Master,
//	 receiving and displaying results
class Prompter extends Actor {
 
	// To add logging of messages received,
	//	 1. edit the application.conf file
	//	 2. use the following line instead of def receive = {
	//def receive = akka.event.LoggingReceive {
	
	def receive = {
		case QueryResult(fracContaining, numTotalPages) => {
			if (numTotalPages > 0) {
				println((fracContaining*100.0) + "% of " + numTotalPages + " total pages matched.")
			}
			
			// Prompt for the next query
			val q = scala.io.StdIn.readLine("Enter a query: ")
			
			sender ! Query(if(q.length == 0) Nil else q.split("(_|\\W)+").map(_.toLowerCase))
		}
	}
}

class Master extends Actor {
	val maxPages = 100

	val urlsToIndex = scala.collection.mutable.HashSet[String]()
	
	// indexedUrls duplicates information with indexedPages,
	//	 but indexedUrls is quick to check for duplicates
	val indexedPages = scala.collection.mutable.ListBuffer[Page]()
	val indexedUrls = scala.collection.mutable.HashSet[String]()
	
	def receive = {
		case StartIndexing(urls) => {
			urls foreach { (url: String) =>
				context.actorOf(Props[Fetcher]) ! IndexRequest(url)
			}
			
			val prompter = context.actorOf(Props[Prompter])
			prompter ! QueryResult(0.0, 0)
		}
		
		case Query(terms) => {
			if (terms.size == 0) {
				context.system.shutdown()
			} else {
				val count: Double = indexedPages.size
				val filtered = indexedPages filter ( _.containsAll(terms) )
				if (count > 0) {
					val fraction: Double = (filtered.size).toDouble / count.toDouble
					sender ! QueryResult(fraction, count.toInt)
				}
				QueryResult(0.0, 0)
			}
		}
	 
		case x: Option[_] => {
			// See if we got a RawPage back
			x match {
				case Some(RawPage(url, html)) => {
					// Add the page to the indexed collections
					val pg = new Page(url, html)
					indexedPages += pg
					indexedUrls += url
			
					// Add the links found in the page to the set of links to index
					urlsToIndex ++= pg.getLinks.filter( link =>
							!urlsToIndex.contains(link) && !indexedUrls.contains(link) )
				}
				case _ => Unit
			}	
			
			// Regardless of whether or not we got a RawPage,
			//	 we should send another request to the Fetcher
			if (!urlsToIndex.isEmpty && indexedUrls.size < maxPages) {
				val nextUrl = urlsToIndex.head
				urlsToIndex.remove(nextUrl)
				sender ! IndexRequest(nextUrl)
			}
			// If urlsToIndex is empty, we should do something to prevent
			//	 the sender Fetcher from remaining idle,
			//	 but that is beyond our scope
		}
				
	}

}




		