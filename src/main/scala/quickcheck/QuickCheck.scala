package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genMap: Gen[Map[Int,Int]] = oneOf(
    const(Map.empty[Int,Int]),
    for {
      k <- arbitrary[Int]
      v <- arbitrary[Int]
      m <- oneOf(const(Map.empty[Int,Int]), genMap)
    } yield m.updated(k, v)
  )

  lazy val genHeap: Gen[H] = for {
    v <- Arbitrary.arbitrary[A]
    m <- Gen.oneOf(Gen.const(empty),   genHeap)
  } yield insert(v, m)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
property("min from 2 in empty") = forAll { (a: A, b: A) =>  //1
  val h = insert(a, insert(b, empty))
  findMin(h) == min(a,b)
}

  property("empty") = forAll { a:A => //2
  isEmpty(deleteMin(insert(a,empty)))
  }

  property("sorted") = forAll { (h: H) => //3
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        val h1 = deleteMin(h)
        isEmpty(h1) || (m <= findMin(h1) && isSorted(h1))
      }
    isSorted(h)
  }

  property("melding heaps") = forAll { (h1:H, h2:H) => //4
    val minH1 = findMin(h1)
    val minH2 = findMin(h2)
    val merged = meld(h1, h2)
    findMin(merged) == (if (minH1 > minH2) minH2 else minH1)
  }
  property("meld") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }


}
