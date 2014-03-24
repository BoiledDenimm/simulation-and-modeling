//@Author Mark Hoefer
import scalation.event.Event
import scalation.event.Model
import scalation.event.Entity
import scalation.random.Randi
import scala.collection.mutable.Queue
import scalation.stat.Statistic

/** AbleBaker CarHop Simulation
 *  
 *  
 *  @author Mark Hoefer
 */
object AbleBakerCarHop extends App{

	val maxCalls = 26
	new AbleBakerCarHopModel("Able Baker Carhop", maxCalls)
}

/** AbleBakerCarHopModel(name:String, nArrivals:Int)
 *
 * @extends Model(name, false)
 */
class AbleBakerCarHopModel(name:String, nArrivals:Int) extends Model(name, false) {
	
	var nArr = 0.0	//state
	var nInA = 0.0	//0 or 1 (Able idle or busy) 
	var nInB = 0.0	//0 or 1 (Baker idle or busy)
	var nIn = 0.0		//number in system
	var nOut = 0.0
	var waitQ: Queue[Entity] = new Queue() //queue for waiting customers
	val t_s_stat = new Statistic()
	val t_y_stat = new Statistic()
	val t_q_stat = new Statistic()
	
	/** Arrival(customer: Entity)
	 *  What happens when a customer arrives at the system
	 *
	 * @extends Event(customer, this)
	 */
	case class Arrival(customer:Entity) extends Event(customer,this){
		override def occur(){
			var serviceT = 0.0
			if(nArr < nArrivals){
				var iArrivalT = iArrivalGen()
				val next2Arrive = new Entity(clock+iArrivalT, 0.0)
				
				println("nextCustomer: "+next2Arrive +" arrivalT: "+(clock+iArrivalT))
				schedule(iArrivalT, Arrival(next2Arrive))
			}
			if (nInA == 0){
				customer.serviceT = serviceGenA()
				println("customer: "+customer +" able serviceT: "+(customer.serviceT))
				schedule(customer.serviceT, DepartureA(customer))
				nInA = 1
			} else if (nInB == 0){
			    customer.serviceT = serviceGenB()
				println("customer: "+customer +" baker serviceT"+(customer.serviceT))
			    schedule(customer.serviceT, DepartureB(customer))
				nInB = 1
			} else {
			
				println("customer: "+customer +" queued in WaitQ")
				waitQ.enqueue(customer)
			}
			nArr += 1
			nIn += 1
		}
	}
	
	/** DepartureA(customer: Entity)
	 *  What happens when a customer departs from service
	 *
	 * @extends Event(customer, this)
	 */
	case class DepartureA(customer:Entity) extends Event(customer, this){
		override def occur(){
			nInA = 0
			
			if (waitQ.isEmpty == false){
				var nextCustomer = waitQ.dequeue
				nextCustomer.serviceT = serviceGenA()
				println("nextCustomer: "+nextCustomer +" able serviceT: "+(nextCustomer.serviceT))
				schedule(nextCustomer.serviceT, DepartureA(nextCustomer))
				nInA=1
			}
			nIn -= 1
			nOut += 1
			t_y_stat.tally(clock - customer.arrivalT)
			//record service time 
			t_s_stat.tally(customer.serviceT)
			t_q_stat.tally((clock - customer.arrivalT)-customer.serviceT)
		}
	}
	
	/** DepartureB(customer: Entity)
	 *  What happens when a customer departs from service
	 *
	 * @extends Event(customer, this)
	 */
	case class DepartureB(customer:Entity) extends Event(customer, this){
		override def occur(){
			nInB = 0
			if (waitQ.isEmpty == false){
			  if(nInA == 0){
			    var nextCustomer = waitQ.dequeue()
					nextCustomer.serviceT = serviceGenA()
					println("nextCustomer: "+nextCustomer +" able serviceT: "+(nextCustomer.serviceT))
					schedule(nextCustomer.serviceT, DepartureA(nextCustomer))
					nInA=1
			  }else{
			    var nextCustomer = waitQ.dequeue()
					nextCustomer.serviceT = serviceGenB()
					println("nextCustomer: "+nextCustomer +"baker serviceT: "+(nextCustomer.serviceT))
					schedule(nextCustomer.serviceT, DepartureB(nextCustomer))
					nInB=1
			  }
			}
			nIn -= 1
			nOut += 1
			t_y_stat.tally(clock - customer.arrivalT)
			//record service time 
			t_s_stat.tally(customer.serviceT)
			t_q_stat.tally((clock - customer.arrivalT)-customer.serviceT)
		}

	}
	
	/** iArrivalGen(): Generates a service time for Able
	 *
	 *  @return	loadT		time it takes Able to serve customer
     */
	private def iArrivalGen(): Double = {
	  val numXGen = Randi(0,99)
	  val numX = numXGen.gen
	  var arrivalT = 0.0
	  if(numX >0 && numX <26){
	    arrivalT = 1.0
	  }else if (numX >25 && numX <66){
	    arrivalT = 2.0
	  } else if (numX >65 && numX <86){
	    arrivalT = 3.0
	  } else{
	    arrivalT = 4.0
	  }
	  return arrivalT
	}
	
	/** serviceGenA(): Generates a service time for Able
	 *
	 *  @return	loadT		time it takes Able to serve customer
     */
	private def serviceGenA(): Double = {
	  val numXGen = Randi(0,99)
	  val numX = numXGen.gen	  
	  var arrivalT = 0.0
	  if(numX >0 && numX <31){
	    arrivalT = 2.0
	  }else if (numX >30 && numX <59){
	    arrivalT = 3.0
	  } else if (numX >58 && numX <84){
	    arrivalT = 4.0
	  } else{
	    arrivalT = 5.0
	  }
	  return arrivalT
	}
	
	/** serviceGenB(): Generates a service time for Baker
	 *
	 *  @return	loadT		time it takes Baker to serve customer
     */
	private def serviceGenB(): Double = {
	  val numXGen = Randi(0,99)
	  val numX = numXGen.gen	  
	  var arrivalT=0.0
	  if(numX >0 && numX <36){
	    arrivalT = 3.0
	  }else if (numX >35 && numX<61){
	    arrivalT = 4.0
	  } else if (numX >60 && numX<81){
	    arrivalT = 5.0
	  } else{
	    arrivalT = 6.0
	  }
	  return arrivalT
	}
	
	schedule(0.0, Arrival(new Entity(0.0, 0.0)))
	simulate(0.0)
	report(Array(("nArr", nArr), ("nIn", nIn), ("nOut", nOut)))
	reports(Array(("t_q", t_q_stat),("t_s", t_s_stat),("t_y", t_y_stat)))
}
	
	