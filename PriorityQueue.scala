import scala.collection.mutable.ArrayBuffer

/** This PriorityQueue Class implements the PriorityQueue interface using
 *  a binary heap.
 *  
 *  @tparam A   type of the elements in the queue
 *  @param ord  implicit ordering used to compare elements of type 'A'
 *  
 *  @author Mark Hoefer
 */
class PriorityQueue[A](implicit val ord: Ordering[A])
{
  import ord._

  /** Defines the attributes of an empty priority queue */
  private var items: ArrayBuffer[A] = new ArrayBuffer()
  private var mySize: Int = 0
  

  /** Inserts a single element into the priority queue.
   *
   *  @param  elem        the element to insert.
   *  @return this    the priority queue object in its current state
   */
  def +=(elem: A): this.type = {
    if(items.isEmpty){
      items.insert(0, null.asInstanceOf[A]) //we don't use index 0, set as null
      items.insert(items.length, elem) //insert first element
      mySize = mySize + 1  //adjust mySize accordingly
      this
    }else{
      items.insert(items.length, elem) 
      mySize = mySize + 1
      var k = mySize
      while(k>1 && (items.apply(k/2).asInstanceOf[A] > elem)){
        items.update(k, items.apply(k/2).asInstanceOf[A])
        k=k/2
      }
      items.update(k, elem)
      this
    }
  }

  /** Returns the element with the highest priority in the queue,
   *  and removes this element from the queue. After the element
   *  is removed, the queue is adjusted to move the elements up in
   *  the queue
   *
   *  @throws Predef.NoSuchElementException
   *  @return top (index 1)
   */
  def dequeue(): A = {
    if (mySize > 0) {
      val top = items.apply(1).asInstanceOf[A]
      items.update(1, items.apply(mySize).asInstanceOf[A])
      mySize = mySize - 1
      if(mySize > 1){
        heapify(1)
      }
      return top
    } else
      throw new NoSuchElementException("no element to remove from heap")
    
  }
  
  /** Updates the queue after the top element is removed.
   *
   *  @return top element   the minimum element currently in the queue
   */
  def peek(): A = items.apply(1)
  
  /** Updates the queue after the top element is removed.
   *
   *  @return mySize    the number of elements currently in the queue
   */
  def length: Int = mySize
  

  /** Updates the queue after the top element is removed.
   *
   *  @return this  the priority queue object in its current state
   */
  def result(): PriorityQueue[A] = this
  
  
  /** Removes all elements from the queue. After this operation is completed,
   *  the queue will be empty.
   *  
   *  @precondition   priority queue in some state
   *  @postcondition  empty priority queue 
   */
  def clear(): Unit = { 
    mySize = 0
    items.clear()
  }
  
  /** Updates the queue after the top element is removed.
   *
   *  @precondition   subheaps of vroot satisfy heap property
   *  @postcondition  heap rooted at vroot satisfies heap property 
   */
  private def heapify(vroot: Int){
    val last = items.apply(vroot).asInstanceOf[A] //last element in queue
    var child = 0
    var k = vroot
    var endLoop = 0
    
    while(2*k <= mySize){
      //find smallest child, first check left, then check right
      child = 2*k
      //right child exists then check right < left
      if(child < mySize && (items.apply((child+1)).asInstanceOf[A] < items.apply(child).asInstanceOf[A] )){
        child = child + 1
      }
      
      //if the last element <= child then we break from the loop and add last at k
      if(last <= items.apply(child).asInstanceOf[A]){
        endLoop = k
        k = mySize*2
      }else{
        //otherwise we add the child at k
        items.update(k, items.apply(child).asInstanceOf[A])
        k=child
        endLoop = child
      }
    }
    k = endLoop
    items.update(k, last)
  }
  def isEmpty(): Boolean = if(length == 0) true else false
}

