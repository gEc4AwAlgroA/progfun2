package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

//  lazy val genMap: Gen[Map[Int,Int]] = for {
//    k <- arbitrary[Int]
//    v <- arbitrary[Int]
//    m <- oneOf(const(Map.empty[Int,Int]), genMap)
//  } yield m.updated(k, v)

//  const() thanks to https://github.com/glebd/scala-course/issues/1
  lazy val genHeap: Gen[H] = for {
    value <- arbitrary[A]
    more <- oneOf(const(empty), genHeap)
  } yield insert(value, more)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minoftwogiveslower") = forAll { (value1: A, value2: A) =>
    val h = insert(value2, insert(value1, empty))
    if (value1 < value2) findMin(h) == value1 else findMin(h) == value2
  }
  property("insertintoemptyanddeletemingivesempty") = forAll { (value: A) =>
    deleteMin(insert(value, empty)) == empty
  }

  property("orderofmins") = forAll { (heap: H, value: A) =>
//    val nonEmpty = insert(value, heap)
    def findMinCarry(h: H, minsofar: A): Boolean = {
      if (isEmpty(h)) true
      else findMin(h) >= minsofar && findMinCarry(deleteMin(h), findMin(h))
    }
//    findMinCarry(deleteMin(nonEmpty), findMin(nonEmpty))
    findMinCarry(deleteMin(heap), findMin(heap))
  }

  property("minofmeldisminofone") = forAll { (heap1: H, heap2: H) =>
    findMin(meld(heap1, heap2)) == findMin(heap1) || findMin(meld(heap1, heap2)) == findMin(heap2)
  }

  property("takeandreturn") = forAll { (heap: H) =>
//    println("A" + heap)
//    println("B" + findMin(heap))
//    println("C" + deleteMin(heap))
//    println("D" + insert(findMin(heap), deleteMin(heap)))
    findMin(heap) == findMin(insert(findMin(heap), deleteMin(heap)))
  }

  property("takeandreturncforderofmins") = forAll { (heap: H) =>
    def findMinList(h: H, mins: List[A]): List[A] = {
      if (isEmpty(h)) mins
      else findMinList(deleteMin(h), findMin(h) :: mins)
    }
    findMinList(heap, List()) == findMinList(insert(findMin(heap), deleteMin(heap)), List())
  }

  property("insertedvaluemustexist") = forAll { (heap: H, value: A) =>
    def findMinList(h: H, mins: List[A]): List[A] = {
      if (isEmpty(h)) mins
      else findMinList(deleteMin(h), findMin(h) :: mins)
    }
    findMinList(insert(value, heap), List()) contains value
  }

//  property("compareinserts") = forAll { (heap: H, value: A) =>
//    println("0" + heap)
//    println("1" + value)
//    println("A" + insert(value, heap))
//    println("B" + insertOrig(value, heap))
//    insert(value, heap) == insertOrig(value, heap)
//  }

//  property("comparedeletes") = forAll { (heap: H) =>
//    deleteMin(heap) == deleteMinOrig(heap)
//  }
}
