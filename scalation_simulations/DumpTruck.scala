import scalation.event.Event
import scalation.event.Model
import scalation.event.Entity
import scalation.random.Randi
import scala.collection.mutable.Queue

/** Dump Truck Problem
 *  
 *  
 *  @author Mark Hoefer
 */
object DumpTruck extends App{

	val maxLoads = 31
	new DumpTruck("Dump Trucks", maxLoads)
}


/** DumpTruck(name:String, nArrivals:Int)
 *
 * @extends Model(name, false)
 */
class DumpTruck(name:String, nArrivals:Int) extends Model(name, false) {
	
	var nArr = 0	//number of trucks loaded
	var nLoading = 0	//0, 1, or 2 (number of trucks being loaded) 
	var nWeighing = 0	//0 or 1 (number of trucks being weighed)
	var loadQ: Queue[Entity] = new Queue() //loading queue
	var scaleQ: Queue[Entity] = new Queue() //weighing queue
	var nIn = 0		//number in system (always 6)
	
	/** Arrival(truck: Entity)
	 *  What happens when a truck arrives at the system
	 *
	 * @extends Event(truck, this)
	 */
	case class Arrival(truck:Entity) extends Event(truck,this){
		override def occur(){
			var serviceT = 0.0
			
			if(nArr < nArrivals){ //loaded < maxLoads
			  
				if(nLoading < 2){ //if loader available
				  truck.serviceT = loadTimeGen() //generate load Time
				  println(truck + " load departure time: " + (truck.serviceT + clock))
				  schedule(truck.serviceT, LoadDeparture(truck)) //schedule Load Departure
				  nLoading += 1
				} else {
					println(truck + " load-queued")
				  loadQ.enqueue(truck) //add to loadQ
				}
			}
			
			nArr += 1
			nIn += 1
		}
	}
	
	/** LoadDeparture(truck: Entity)
	 *  What happens when a truck departs from loading
	 *
	 * @extends Event(truck, this)
	 */
	case class LoadDeparture(truck:Entity) extends Event(truck, this){
		override def occur(){
			nLoading -= 1
			
			if(nWeighing<1){ //if scale is open
			  truck.serviceT = weighTimeGen() //generate scale service time
			  println(truck + " weigh departure time: " + (truck.serviceT + clock))
			  schedule(truck.serviceT, ScaleDeparture(truck)) //schedule scale departure
			  nWeighing += 1
			} else {
				println(truck + " added to scale queue")
			  scaleQ.enqueue(truck) //add to scaleQ
			}
			
			if (loadQ.isEmpty == false){
				var nextTruck = loadQ.dequeue() //get the next truck in line to be loaded
				nextTruck.serviceT = loadTimeGen() //generate load service time
				println(nextTruck + " load departure time: " + (nextTruck.serviceT + clock))
				schedule(nextTruck.serviceT, LoadDeparture(nextTruck)) //schedule next load departure
				nLoading += 1
			}
		}
	}
	
	/** ScaleDeparture(truck: Entity) 
	 *  What happens when a truck departs from the scale
	 *
	 *  @params	truck		truck that is departing from the scale
     */
	case class ScaleDeparture(truck:Entity) extends Event(truck, this){
		override def occur(){
			nWeighing -= 1
			if (scaleQ.isEmpty == false){
				var nextTruck = scaleQ.dequeue() //get the next truck in line for the scale
				nextTruck.serviceT = weighTimeGen() //generate scale service time
				println(nextTruck + " weigh departure time " + (nextTruck.serviceT + clock))
				schedule(nextTruck.serviceT, ScaleDeparture(nextTruck)) //schedule next scale departure
				nWeighing += 1
			}
			
			truck.serviceT = travelTimeGen() //generate travel time
			println(truck + " arrival time: " + (truck.serviceT + clock))
			schedule(truck.serviceT, Arrival(truck)) //schedule arrival at end of travel time
		}
		
	}
	
	/** Generates a service time for loading a truck
	 *
	 *  @return	loadT		time it takes to load the truck
     */
	private def loadTimeGen(): Double = {
	  val numXGen = Randi(0,9)
	  val numX = numXGen.gen
	  var loadT = 0.0
	  if(numX>0 && numX<4){
	    loadT = 5.0
	  }else if (numX>3 && numX<9){
	    loadT = 10.0
	  } else{
	    loadT = 15.0
	  }
	  return loadT
	}
	
	/** Generates a service time for weighing a truck
	 *
	 *  @return	scaleT		time truck spends on the scale
     */
	private def weighTimeGen(): Double = {
	  val numXGen = Randi(0,9)
	  val numX = numXGen.gen
	  var scaleT = 0.0
	  if(numX>0 && numX<8){
	    scaleT = 12.0
	  } else{
	    scaleT = 16.0
	  }
	  return scaleT
	}
	
	/** Generates a service time for the truck's travels
	 *
	 *  @return	travelT		time it takes the truck to deliver its load and return to be reloaded
     */
	private def travelTimeGen(): Double = {
	  val numXGen = Randi(0,99)
	  val numX = numXGen.gen
	  var travelT=0.0
	  if(numX>0 && numX<5){
	    travelT = 40.0
	  }else if (numX>4 && numX<8){
	    travelT = 60.0
	  } else if (numX>7 && numX<10){
	    travelT = 80.0
	  } else{
	    travelT = 100.0
	  }
	  return travelT
	}
	
	schedule(0.0, Arrival(new Entity(0.0, 0.0)))
	schedule(0.0, Arrival(new Entity(0.0, 0.0)))
	schedule(0.0, Arrival(new Entity(0.0, 0.0)))
	schedule(0.0, Arrival(new Entity(0.0, 0.0)))
	schedule(0.0, Arrival(new Entity(0.0, 0.0)))
	schedule(0.0, Arrival(new Entity(0.0, 0.0)))
	simulate(0.0)

}