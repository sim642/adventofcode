package eu.sim642.adventofcode2017

import Day17._
import eu.sim642.AdventOfCodeSuite
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class Day17Test extends FunSuite with PropertyChecks with AdventOfCodeSuite {

  test("Part 1 example states") {
    import Part1._

    val states = Table(
      "state",
      SpinState(Vector(0), 0),
      SpinState(Vector(0, 1), 1),
      SpinState(Vector(0, 2, 1), 1),
      SpinState(Vector(0, 2, 3, 1), 2),

      SpinState(Vector(0, 2, 4, 3, 1), 2),
      SpinState(Vector(0, 5, 2, 4, 3, 1), 1),
      SpinState(Vector(0, 5, 2, 4, 3, 6, 1), 5),
      SpinState(Vector(0, 5, 7, 2, 4, 3, 6, 1), 2),
      SpinState(Vector(0, 5, 7, 2, 4, 3, 8, 6, 1), 6),
      SpinState(Vector(0, 9, 5, 7, 2, 4, 3, 8, 6, 1), 1)
    )

    val it = spinlock(3)
    forAll (states) { state =>
      assert(it.next() == state)
    }
  }

  test("Part 1 example") {
    assert(Part1.spinlockAfter(3) == 638)
  }

  test("Part 1 input answer") {
    assert(Part1.spinlockAfter(input) == 355)
  }

  test("Part 2 input answer") {
    assert(Part2.spinlockAfter(input) == 6154117)
  }
}
