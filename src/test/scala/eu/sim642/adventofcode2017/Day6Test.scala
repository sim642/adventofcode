package eu.sim642.adventofcode2017

import Day6._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class Day6Test extends FunSuite with PropertyChecks {

  test("reallocCycle") {
    assert(reallocCycle(IndexedSeq(0, 0, 0, 1, 0, 0)) == IndexedSeq(0, 0, 0, 0, 1, 0))
    assert(reallocCycle(IndexedSeq(0, 0, 0, 2, 0, 0)) == IndexedSeq(0, 0, 0, 0, 1, 1))
    assert(reallocCycle(IndexedSeq(0, 0, 0, 3, 0, 0)) == IndexedSeq(1, 0, 0, 0, 1, 1))
    assert(reallocCycle(IndexedSeq(0, 0, 0, 4, 0, 0)) == IndexedSeq(1, 1, 0, 0, 1, 1))
    assert(reallocCycle(IndexedSeq(0, 0, 0, 5, 0, 0)) == IndexedSeq(1, 1, 1, 0, 1, 1))
    assert(reallocCycle(IndexedSeq(0, 0, 0, 6, 0, 0)) == IndexedSeq(1, 1, 1, 1, 1, 1))
    assert(reallocCycle(IndexedSeq(0, 0, 0, 7, 0, 0)) == IndexedSeq(1, 1, 1, 1, 2, 1))
    assert(reallocCycle(IndexedSeq(0, 0, 0, 8, 0, 0)) == IndexedSeq(1, 1, 1, 1, 2, 2))
    assert(reallocCycle(IndexedSeq(0, 0, 0, 9, 0, 0)) == IndexedSeq(2, 1, 1, 1, 2, 2))
    assert(reallocCycle(IndexedSeq(0, 0, 0, 10, 0, 0)) == IndexedSeq(2, 2, 1, 1, 2, 2))
  }

  test("Part 1 examples") {
    val memories = Table(
      "memory",
      IndexedSeq(0, 2, 7, 0),
      IndexedSeq(2, 4, 1, 2),
      IndexedSeq(3, 1, 2, 3),
      IndexedSeq(0, 2, 3, 4),
      IndexedSeq(1, 3, 4, 1),
      IndexedSeq(2, 4, 1, 2)
    )
    val it = new ReallocIterator(memories.head)
    forAll (memories) { memory =>
      assert(it.next() == memory)
    }

    assert(reallocCycleCount(IndexedSeq(0, 2, 7, 0)) == 5)
  }

  test("Part 1 input answer") {
    assert(reallocCycleCount(inputSeq) == 3156)
  }
}
