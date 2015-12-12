package linkedlist

import scala.collection.mutable

class LinkedNode[T](var value: T, var  next:  LinkedNode[T], var index: Int)

class LinkedList[T] extends mutable.Iterable[T] {

  private var listLength = 0
  private var firstNode: LinkedNode[T] = null
  private var lastNode: LinkedNode[T] = null

  /**
    * builds a LinkedList from a variable number of elements
    * @param elem the elements to be added to the list
    */
  def this(elem: T*) = {
    this()
    if (elem.nonEmpty)
      insertAll(elems = elem)
  }

  /**
    * builds a LinkedList from an existing Traversable
    * @param elems the Traversable to be used
    */
  def this(elems: Traversable[T]) = {
    this()
    insertAll(elems = elems)
  }

  /**
    * updates the value of an element
    * @param n the index of the element
    * @param newelem the new value
    */
  def update(n: Int, newelem: T) = findByIndex(n).value = newelem

  private def findByIndex(i: Int) = {
    if (i >= listLength)
      throw new IndexOutOfBoundsException(s"Index out of bounds: $i, list size: $listLength")

    var node = firstNode

    while (node.index < i)
      node = node.next

    node
  }

  /**
    * gets an iterator for the list
    * @return an instance of the iterator
    */
  override def iterator: Iterator[T] = {
    new Iterator[T] {
      private var linkedNode = firstNode

      override def hasNext: Boolean = linkedNode != null

      override def next(): T = {
        if (!hasNext)
          throw new IndexOutOfBoundsException("Trying to iterate over bounds")
        val ret = linkedNode.value
        linkedNode = linkedNode.next
        ret
      }
    }
  }

  /**
    * empties a list
    */
  def clear(): Unit = {
    listLength -= listLength
    firstNode = null
    lastNode = null
  }

  /**
    * Appends a Traversable at the end of a linked list
    * @param list the list to be appended
    */
  def ++=(list: Traversable[T]): Unit = list.foreach(elem => this += elem)


  /**
    * appends an element to the list
    * @param elem the element to be appended
    */
  def +=(elem: T): Unit = {
    val node = new LinkedNode[T](elem, null, 0)
    if (listLength == 0) {
      firstNode = node
    } else {
      node.index = listLength
      lastNode.next = node
    }
    lastNode = node
    listLength += 1
  }

  /**
    * checks whether an element is contained in the array
    * @param value the value we want to look for
    * @return true if the value is in the array, false otherwise
    */
  def contains(value: T): Boolean = {
    foreach(elem => if (elem.equals(value)) return true)
    false
  }

  /**
    * returns a list containing the difference between this list and another one
    * @param list the list we want to use for comparison
    * @return a list containing the difference
    */
  def diff(list: Traversable[T]) = filterNot(elem => list.exists(x => x.equals(elem)))

  /**
    * removes an element from the list
    * @param elem the element to be removed
    */
  def -=(elem: T): Unit = {
    remove(findByValue(elem))
  }

  /**
    * removes an element by index
    * @param n the index to be removed
    */
  def remove(n: Int) = {
    val node = findByIndex(n)
    // Change the index of all the successor nodes
    changeIndex(-1, node)
    // Change the successor of the predecessor of the node we're removing
    // to the successor of the node we're removing
    findByIndex(n - 1).next = node.next
    listLength -= 1
  }

  /**
    * finds the element at the given index
    * @param n the index to be used
    * @return the element at the index given
    */
  def apply(n: Int): T = findByIndex(n).value

  /**
    * prepends an element to the list
    * @param elem the element to be prepended
    */
  def push(elem: T) = :+=(elem)

  /**
    * prepends an element to the list
    * @param elem the element to be prepended
    */
  def :+=(elem: T): Unit = {
    val node = new LinkedNode[T](elem, null, 0)
    if (listLength == 0) {
      lastNode = node
    } else {
      val prevHead = firstNode
      changeIndex(1, prevHead)
      node.next = firstNode
    }
    firstNode = node
    listLength += 1
  }

  /**
    * inserts all the elements contained in a list into this list
    * @param startIndex the index after which we want to insert the list elements, default = 0
    * @param elems the list of elements to be inserted
    */
  def insertAll(startIndex: Int = 0, elems: Traversable[T]): Unit = {
    if (elems.isEmpty)
      return
    if (listLength == 0) {
      elems.foreach(elem => this += elem)
    } else {
      val currentNode = findByIndex(startIndex)
      insertAllHelper(currentNode.index, currentNode, currentNode.next, elems)
    }
  }

  /** Some helper methods that allow to write easily remove, insertion methods **/
  private def insertAllHelper(startIndex: Int = 0, startNode: LinkedNode[T] = firstNode, nextNode: LinkedNode[T] = null,
    elems: Traversable[T]) = {
    var currentNode = startNode
    var i = startIndex
    for (elem <- elems) {
      i += 1
      listLength += 1
      currentNode.next = new LinkedNode[T](elem, nextNode, i)
      currentNode = currentNode.next
    }
    if (nextNode != null)
      nextNode.index += i
  }

  private def changeIndex(modifier: Int, startNode: LinkedNode[T] = firstNode) = {
    var linkedNode = startNode
    while (linkedNode.next != null) {
      linkedNode.index += modifier
      linkedNode = linkedNode.next
    }
  }

  /**
    * removes the first element of the list and returns it
    * @return the first element of the list
    */
  def pop(): T = {
    val h = apply(0)
    this -= h
    h
  }

  /**
    * returns all the elements in the list minus the first one
    * @return a buffer containing the tail of the list
    */
  override def tail() = iterator.drop(1).toBuffer

  /**
    * @return the size of the list
    */
  override def size = listLength

  /**
    * retrieves the head of the list
    * @return the head of the list
    */
  override def head = firstNode.value

  /**
    * finds all the values that match a predicate
    * @param predicate the predicate to be used when matching
    * @return a LinkedList containing all the elements that matched the predicate
    */
  override def filter(predicate: (T) => Boolean) = {
    val list = new LinkedList[T]()
    foreach(elem => if (predicate(elem)) list += elem)
    list
  }

  /**
    * finds all the values that don't match a predicate
    * @param predicate the predicate to be used when matching
    * @return a LinkedList containing all the elements that don't match the predicate
    */
  override def filterNot(predicate: (T) => Boolean) = {
    val list = new LinkedList[T]()
    foreach(elem => if (!predicate(elem)) list += elem)
    list
  }

  /**
    * checks whether this list equals another object
    * @param other the object to be used for comparison
    * @return true if the length matches and if all the elements are equal else false
    */
  override def equals(other: Any): Boolean = other match {
    case that: LinkedList[T] =>
      (that canEqual this) &&
      listLength == that.listLength && toSet == that.toSet
    case _ => false
  }

  // Automatically Generated
  override def canEqual(other: Any): Boolean = other.isInstanceOf[LinkedList[T]]

  def reverse(): LinkedList[T] = {
    val list = new LinkedList[T]()
    foreach(elem => list :+= elem)
    list
  }

  // Automatically Generated
  override def hashCode(): Int = {
    val state = Seq(listLength)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  private def findByValue(value: T): Int = iterator.indexOf(value)

}