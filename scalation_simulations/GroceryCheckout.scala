
import scalation.event.{Entity, Event, Model}
import scalation.random.{Exponential, Variate}
import scalation.stat.Statistic
import scala.collection.mutable.Queue

object GroceryCheckout extends App{
	val lambda = 6.0
	val mu = 7.5
	val maxCustomers = 1000
	val iArrivalRV = Exponential(3600/lambda, 1)
	val serviceRV = Exponential(3600/mu, 1)
	new GroceryCheckoutModel("Grocery Checkout Line", maxCustomers, iArrivalRV, serviceRV)
}

class GroceryCheckoutModel(name:String, nArrivals:Int,iArrivalRV:Variate, serviceRV: Variate) extends Model(name, false){
	
	var nArr = 0.0	//state
	var nIn = 0.0		//nIn system
	var nOut = 0.0	//number departed
	var waitQ = new Queue[Entity]()
	val t_s_stat = new Statistic()
	val t_y_stat = new Statistic()
	val l_q_stat = new Statistic()
	val t_q_stat = new Statistic()
	val l_s_stat = new Statistic()
	val l_y_stat = new Statistic()
	case class Arrival(customer:Entity) extends Event(customer,this){
		override def occur(){
			//if end condition not reached, generate new arrival
			if(nArr < nArrivals -1){
				val iArrivalT = iArrivalRV.gen
				val next2Arrive = Entity(clock + iArrivalT, serviceRV.gen)
				schedule(iArrivalT, Arrival(next2Arrive))
			}
			//if teller open, schedule departure
			if(nIn == 0){
				schedule(customer.serviceT, Departure(customer))
			}
			//queue the event
			else{
				waitQ += customer
			}	
			nArr += 1
			nIn += 1
			// l_s_stat.tally(nIn - waitQ.length)
			// l_q_stat.tally(waitQ.length)
			// l_y_stat.tally(nIn)
		}
	}
	case class Departure(customer:Entity) extends Event(customer, this){
		override def occur(){
			if(nIn > 1){
				val next2Depart = waitQ.dequeue
				//record time in service stat
				schedule(next2Depart.serviceT, Departure(next2Depart))
			}
			nIn -= 1
			nOut += 1
			// l_q_stat.tally(waitQ.length)
			//record time in system for departing customer
			t_y_stat.tally((clock - customer.arrivalT)/3600)
			//record service time 
			t_s_stat.tally((customer.serviceT)/3600)
			t_q_stat.tally(((clock - customer.arrivalT)-customer.serviceT)/3600)
			// l_y_stat.tally(nIn)
			// l_s_stat.tally(nIn - waitQ.length)
		}
	}
	schedule(0.0, Arrival(new Entity(0.0, serviceRV.gen)))
	simulate(0.0)
	report(Array(("nArr", nArr), ("nIn", nIn), ("nOut", nOut)))
	reports(Array(("t_q", t_q_stat),("t_s", t_s_stat),("t_y", t_y_stat)))  
}