
/* arrivalT is the time the arrival event should occur, so add the clock time to it
* serviceT is how long the service actually takes
* @author Will Henry
*/
class Entity(val arrivalT: Double, val serviceT: Double) {
	val eid = Entity.getEid
	override def toString(): String = "Entity :" + eid + ": Arrival: " + arrivalT + "\tService: " + serviceT
}

//singleton object that increments each eid for created Entitys
object Entity{
	var count = 0

	def getEid(): Int = {
		count = count+ 1
		count
	}
}