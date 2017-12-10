package eu.sim642.adventofcode2017

import Day10._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class Day10Test extends FunSuite with PropertyChecks {

  val exampleInitialState = KnotState(Vector(0, 1, 2, 3, 4))

  test("Part 1 example states") {
    val lengthStates = Table(
      ("length", "state"),
      (3, KnotState(Vector(2, 1, 0, 3, 4), 3, 1)),
      (4, KnotState(Vector(4, 3, 0, 1, 2), 3, 2)),
      (1, KnotState(Vector(4, 3, 0, 1, 2), 1, 3)),
      (5, KnotState(Vector(3, 4, 2, 1, 0), 4, 4))
    )

    var currentState = exampleInitialState
    forAll (lengthStates) { (length, state) =>
      currentState = currentState.reverse(length)
      assert(currentState == state)
    }
  }

  test("Part 1 example hash") {
    assert(simulate(exampleInitialState, Seq(3, 4, 1, 5)).hash == 12)
  }

  test("Part 1 input answer") {
    assert(simulate(KnotState(), inputLengths).hash == 46600)
  }
}
