/*
* @author Will Henry
*/
class Event(val entity: Entity, val director: Model) extends Ordered[Event] with PQItem{
	def compare(that:Event) = this.actTime compare that.actTime
  
  override def toString = entity.toString		
	
	def occur() = println("You must implement an occur method")

}