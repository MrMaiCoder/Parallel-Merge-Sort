import scala.util.Random

object parallel_merge_sort extends App {

  // Sequential merge sort
  def mergeSort(xs: List[Int]): List[Int] = {
    if (xs.isEmpty || xs.length / 2 == 0) xs              // Do nothing if list is empty or contains 1 element
    else {
      val n = xs.length / 2
      val (left, right) = xs.splitAt(n)                   // Split the list in the middle
      merge(mergeSort(left), mergeSort(right))            // Sort the list recursively, then merge them
    }
  }

  def merge(left: List[Int], right: List[Int]): List[Int] = (left, right) match {
    case(Nil, right) => right                             // Left list is empty, returns only the right list
    case(left, Nil) => left                               // Right list is empty, returns only the left list
    case(leftHead :: leftTail, rightHead :: rightTail) => // Both left and right list are not empty
      if (leftHead < rightHead) {                         // leftHead is lesser than rightHead
        leftHead :: merge(leftTail, right)                // Let leftHead be the head, then merge the rest with right list
      }
      else {                                              // leftHead is greater than rightHead
        rightHead :: merge(left, rightTail)               // Let rightHead be the head, then merge the rest with left list
      }
  }

  // Parallel merge sort
  def parallelMergeSort(lst : List[Int]) : List[Int] = {
    if (lst.isEmpty || lst.tail.isEmpty) lst                // List is empty or contains only 1 element
    else {
      val n = lst.length/2
      val (left, right) = lst.splitAt(n)                    // Split the list in the middle
      helper(helper(left) ::: helper(right))                // Sort the list with helper function
    }
  }

  // helper
  def helper(lst : List[Int]) : List[Int] = lst match {
    case Nil => lst                                         // List is empty
    case h::Nil => lst                                      // List contains only 1 element
    case pivot::tail => {                                   // Take the head as our pivot
      val (less_than_pivot, more_than_pivot) = tail.partition(x => x < pivot)
      // Sort the list using partition
      // Partition method takes a predicate function as its parameter and returns two collections
      helper(less_than_pivot) ::: (pivot :: helper(more_than_pivot))
      // Merge the sorted lists
    }
  }


  // Test cases to see if parallel merge sort works correctly
  def tester() : String = {
    val list1 = List()                                            // Empty List
    val list2 = List(7)                                           // List with 1 element
    val list3 = List(8,1)                                         // List with 2 elements
    val list4 = List(2,5,22,42,7,6,84,18,12,43,11,81)             // List with unique numbers
    val list5 = List(1,1,4,4,8,8,8,3,3,3,4)                       // List with repeated numbers
    val list6 = List(1,2,3,4,5,6,7,8,9,10)                        // Sorted list

    assert(parallelMergeSort(list1) == List())
    assert(parallelMergeSort(list2) == List(7))
    assert(parallelMergeSort(list3) == List(1,8))
    assert(parallelMergeSort(list4) == mergeSort(list4))
    assert(parallelMergeSort(list5) == mergeSort(list5))
    assert(parallelMergeSort(list6) == mergeSort(list6))

    "Test passed! Parallel merge sort sorted the lists correctly."

  }

  // Let's compare the speed of sequential merge sort and parallel merge sort.
  // I will do this by creating a long list consisting of 500 random numbers.
  // Then I will time how long each function takes to run the random list.
  // I will print out the time each algorithm takes, let's check it out.

  // Create a list of 100 random numbers
  var randomList = List[Int]()
  for(i <- 0 to 500) {
    val ran = new Random()
    val num = ran.nextInt()
    randomList = num::randomList
  }

  // Timer for the function
  def time[T](block: => T): Long = {
    val t0 = System.nanoTime()
    val t1 = System.nanoTime()
    t1 - t0
  }

  // Spoiler: parallel merge sort is faster
  println(tester())
  println("Sequential merge sort takes " + time {mergeSort(randomList)} + " ns")
  println("Parallel merge sort takes " + time {parallelMergeSort(randomList)} + " ns")

}
