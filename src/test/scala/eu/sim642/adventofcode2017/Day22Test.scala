package eu.sim642.adventofcode2017

import Day22._
import eu.sim642.adventofcode2017.Day3.Pos
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class Day22Test extends FunSuite with PropertyChecks {

  val exampleInput = """..#
                       |#..
                       |...""".stripMargin

  test("Part 1 example states") {
    val states = Table(
      "state",
      InfectionState(Set(Pos(-1, 0), Pos(1, 1)), Pos(0, 0), Pos(0, 1)),
      InfectionState(Set(Pos(-1, 0), Pos(1, 1), Pos(0, 0)), Pos(-1, 0), Pos(-1, 0)),
      InfectionState(Set(Pos(1, 1), Pos(0, 0)), Pos(-1, 1), Pos(0, 1)),
      InfectionState(Set(Pos(1, 1), Pos(0, 0), Pos(-1, 1)), Pos(-2, 1), Pos(-1, 0)),
      InfectionState(Set(Pos(1, 1), Pos(0, 0), Pos(-1, 1), Pos(-2, 1)), Pos(-2, 0), Pos(0, -1)),
      InfectionState(Set(Pos(1, 1), Pos(0, 0), Pos(-1, 1), Pos(-2, 1), Pos(-2, 0)), Pos(-1, 0), Pos(1, 0)),
      InfectionState(Set(Pos(1, 1), Pos(0, 0), Pos(-1, 1), Pos(-2, 1), Pos(-2, 0), Pos(-1, 0)), Pos(-1, 1), Pos(0, 1)),
      InfectionState(Set(Pos(1, 1), Pos(0, 0), Pos(-2, 1), Pos(-2, 0), Pos(-1, 0)), Pos(0, 1), Pos(1, 0))
    )

    val it = iterateBursts(exampleInput)
    forAll (states) { state =>
      assert(it.next() == state)
    }
  }

  test("Part 1 infection bursts") {
    val burstInfections = Table(
      ("bursts", "infections"),
      (7, 5),
      (70, 41),
      (10000, 5587)
    )

    forAll (burstInfections) { (bursts, infections) =>
      assert(infectionBursts(exampleInput, bursts) == infections)
    }
  }

  test("Part 1 input answer") {
    assert(infectionBursts(input) == 5460)
  }
}
