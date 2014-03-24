/*
* @author Will Henry
*/
class Model(name: String){
	var eventList = new PriorityQueue[Event]()
	var clock = 0.0
	def schedule(timeDelay: Double, event: Event) = {
		event.actTime = clock + timeDelay
		eventList += event
	} 

	def simulate(clockStart: Double) = {
		clock = clockStart
		while(!eventList.isEmpty){
			val ev = eventList.dequeue
			clock = ev.actTime
			println("Clock: " + clock + "; Processing event -> " + ev);
			ev.occur()

		}
	}


}

