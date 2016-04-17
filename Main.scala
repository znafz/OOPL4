import akka.actor.ActorSystem
import akka.actor.Props 

object Main {
	def main(args: Array[String]): Unit = {
    	//create an actor system
    	val system = ActorSystem("indexing-system")
		val master = system.actorOf(Props[Master], name="master")
		master ! StartIndexing(args)
  	}
}