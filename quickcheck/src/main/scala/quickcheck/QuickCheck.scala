package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  //  lazy val genHeap: Gen[H] = ???
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      a <- arbitrary[Int]
      h <- oneOf(empty, insert(a, empty))
    } yield h
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // would like to check behaviour when calling delete for empty heap
  //  property("reallyEmpty") = forAll { h: H =>
  //    val e = if (isEmpty(h)) 0 else deleteMin(h)
  //    e == h
  //  }

  // insert one Int into empty heap, delete and check that heap is empty
  property("insert Int into empty and delete -> should be empty") = forAll { a: A =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  // insert two Ints, delete min, check that larger is current min
  property("insert 3 Ints and delete -> findMin should return 2nd largest") = forAll { (a: A, b: A, c: A) =>
    val middleInt = List(a, b, c).sorted.tail.head
    val h = deleteMin(insert(c, insert(a, insert(b, empty))))
    middleInt == findMin(h)
  }

  // insert two Ints, check that findMin returns min
  property("insert two Ints -> findMin returns smaller") = forAll { (a: A, b: A) =>
    val minInt = a min b
    val h = insert(a, insert(b, empty))
    //    println(s"a $a b $b max $maxInt h $h")
    minInt == findMin(h)
  }

  // generate list and push it to heap, check that it is returned in sorted order
  property("heap is sorted") = forAll { as: List[A] =>
    @scala.annotation.tailrec
    def loop(ns: List[A], h: H): H = ns match {
      case Nil => h
      case x :: xs => loop(xs, insert(x, h))
    }

    val heap = loop(as, empty)

    def heapToList(heap: H): List[A] = {
      @scala.annotation.tailrec
      def loop(h: H, acc: List[A]): List[A] = h match {
        case h if isEmpty(h) => acc.reverse // sorted high-low so must reverse
        case h => loop(deleteMin(h), findMin(h) :: acc)
      }

      loop(heap, List())
    }

    val heapAsList = heapToList(heap)
    as.sorted == heapAsList

  }

}
