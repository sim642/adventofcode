package eu.sim642.adventofcode2017

import Day5._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite

class Day5Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  test("Part 1 examples") {
    import Part1._

    //noinspection RedundantDefaultArgument
    val states = Table(
      "state",
      OffsetState(Seq(0, 3, 0, 1, -3), 0),
      OffsetState(Seq(1, 3, 0, 1, -3), 0),
      OffsetState(Seq(2, 3, 0, 1, -3), 1),
      OffsetState(Seq(2, 4, 0, 1, -3), 4),
      OffsetState(Seq(2, 4, 0, 1, -2), 1)
    )
    val it = new OffsetIterator(states.head)
    forAll (states) { state =>
      assert(it.next() == state)
    }

    assert(exitSteps("0\n3\n0\n1\n-3") == 5)
  }

  test("Part 1 input answer") {
    assert(Part1.exitSteps(input) == 376976)
  }

  test("Part 2 examples") {
    import Part2._
    assert(exitSteps("0\n3\n0\n1\n-3") == 10)
  }

  test("Part 2 input answer") {
    assert(Part2.exitSteps(input) == 29227751)
  }
}
