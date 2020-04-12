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
    val h = deleteMin(insert(a, insert(b, empty)))
    //    println(s"a $a b $b max $middleInt h $h")
    middleInt == findMin(h)
  }

  // insert two Ints, check that findMin returns min
  property("insert two Ints -> findMin returns smaller") = forAll { (a: A, b: A) =>
    val minInt = a min b
    val h = insert(a, insert(b, empty))
    //    println(s"a $a b $b max $maxInt h $h")
    minInt == findMin(h)
  }


}
