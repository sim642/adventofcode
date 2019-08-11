package eu.sim642.adventofcode2017

import eu.sim642.adventofcode2017.Day6._
import eu.sim642.adventofcode2017.Day6Test._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Suites}

class Day6Test extends Suites(
  new BaseTest,
  new NaiveSolutionTest,
  new NaiverSolutionTest,
  new FloydSolutionTest,
  new BrentSolutionTest,
)

object Day6Test {

  class BaseTest extends FunSuite with PropertyChecks {
    test("reallocCycle") {
      val nextMemories = Table(
        ("memory", "nextMemory"),
        (IndexedSeq(0, 0, 0, 1, 0, 0), IndexedSeq(0, 0, 0, 0, 1, 0)),
        (IndexedSeq(0, 0, 0, 2, 0, 0), IndexedSeq(0, 0, 0, 0, 1, 1)),
        (IndexedSeq(0, 0, 0, 3, 0, 0), IndexedSeq(1, 0, 0, 0, 1, 1)),
        (IndexedSeq(0, 0, 0, 4, 0, 0), IndexedSeq(1, 1, 0, 0, 1, 1)),
        (IndexedSeq(0, 0, 0, 5, 0, 0), IndexedSeq(1, 1, 1, 0, 1, 1)),
        (IndexedSeq(0, 0, 0, 6, 0, 0), IndexedSeq(1, 1, 1, 1, 1, 1)),
        (IndexedSeq(0, 0, 0, 7, 0, 0), IndexedSeq(1, 1, 1, 1, 2, 1)),
        (IndexedSeq(0, 0, 0, 8, 0, 0), IndexedSeq(1, 1, 1, 1, 2, 2)),
        (IndexedSeq(0, 0, 0, 9, 0, 0), IndexedSeq(2, 1, 1, 1, 2, 2)),
        (IndexedSeq(0, 0, 0, 10, 0, 0), IndexedSeq(2, 2, 1, 1, 2, 2))
      )

      forAll (nextMemories) { (memory, nextMemory) =>
        assert(reallocCycle(memory) == nextMemory)
      }
    }
  }

  sealed abstract class SolutionTest(solution: Solution) extends FunSuite {
    test("Part 1 example") {
      assert(solution.reallocCycleCount(IndexedSeq(0, 2, 7, 0)) == 5)
    }

    test("Part 1 input answer") {
      assert(solution.reallocCycleCount(inputSeq) == 3156)
    }

    test("Part 2 example") {
      assert(solution.reallocCycleLoop(IndexedSeq(0, 2, 7, 0)) == 4)
    }

    test("Part 2 input answer") {
      assert(solution.reallocCycleLoop(inputSeq) == 1610)
    }
  }

  class NaiveSolutionTest extends SolutionTest(NaiveSolution) with PropertyChecks {
    test("Part 1 example states") {
      val memories = Table(
        "memory",
        IndexedSeq(0, 2, 7, 0),
        IndexedSeq(2, 4, 1, 2),
        IndexedSeq(3, 1, 2, 3),
        IndexedSeq(0, 2, 3, 4),
        IndexedSeq(1, 3, 4, 1),
        IndexedSeq(2, 4, 1, 2)
      )
      val it = new NaiveSolution.ReallocIterator(memories.head)
      forAll (memories) { memory =>
        assert(it.next() == memory)
      }
    }
  }

  class NaiverSolutionTest extends SolutionTest(NaiverSolution)

  class FloydSolutionTest extends SolutionTest(FloydSolution)

  class BrentSolutionTest extends SolutionTest(BrentSolution)
}
