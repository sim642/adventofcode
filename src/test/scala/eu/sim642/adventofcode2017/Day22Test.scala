package eu.sim642.adventofcode2017

import Day22._
import eu.sim642.adventofcodelib.pos.Pos
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite

class Day22Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  val exampleInput = """..#
                       |#..
                       |...""".stripMargin

  test("Part 1 example states") {
    import Part1._

    val states = Table(
      "state",
      InfectionState(Map(Pos(-1, 0) -> Infected, Pos(1, 1) -> Infected), Pos(0, 0), Pos(0, 1)),
      InfectionState(Map(Pos(-1, 0) -> Infected, Pos(1, 1) -> Infected, Pos(0, 0) -> Infected), Pos(-1, 0), Pos(-1, 0)),
      InfectionState(Map(Pos(1, 1) -> Infected, Pos(0, 0) -> Infected), Pos(-1, 1), Pos(0, 1)),
      InfectionState(Map(Pos(1, 1) -> Infected, Pos(0, 0) -> Infected, Pos(-1, 1) -> Infected), Pos(-2, 1), Pos(-1, 0)),
      InfectionState(Map(Pos(1, 1) -> Infected, Pos(0, 0) -> Infected, Pos(-1, 1) -> Infected, Pos(-2, 1) -> Infected), Pos(-2, 0), Pos(0, -1)),
      InfectionState(Map(Pos(1, 1) -> Infected, Pos(0, 0) -> Infected, Pos(-1, 1) -> Infected, Pos(-2, 1) -> Infected, Pos(-2, 0) -> Infected), Pos(-1, 0), Pos(1, 0)),
      InfectionState(Map(Pos(1, 1) -> Infected, Pos(0, 0) -> Infected, Pos(-1, 1) -> Infected, Pos(-2, 1) -> Infected, Pos(-2, 0) -> Infected, Pos(-1, 0) -> Infected), Pos(-1, 1), Pos(0, 1)),
      InfectionState(Map(Pos(1, 1) -> Infected, Pos(0, 0) -> Infected, Pos(-2, 1) -> Infected, Pos(-2, 0) -> Infected, Pos(-1, 0) -> Infected), Pos(0, 1), Pos(1, 0))
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
      assert(Part1.infectionBursts(exampleInput, bursts) == infections)
    }
  }

  test("Part 1 input answer") {
    assert(Part1.infectionBursts(input) == 5460)
  }

  test("Part 2 example states") {
    import Part2._

    val states = Table(
      "state",
      InfectionState(Map(Pos(-1, 0) -> Infected, Pos(1, 1) -> Infected), Pos(0, 0), Pos(0, 1)),
      InfectionState(Map(Pos(-1, 0) -> Infected, Pos(1, 1) -> Infected, Pos(0, 0) -> Weakened), Pos(-1, 0), Pos(-1, 0)),
      InfectionState(Map(Pos(-1, 0) -> Flagged, Pos(1, 1) -> Infected, Pos(0, 0) -> Weakened), Pos(-1, 1), Pos(0, 1)),

      InfectionState(Map(Pos(-1, 0) -> Flagged, Pos(1, 1) -> Infected, Pos(0, 0) -> Weakened, Pos(-1, 1) -> Weakened), Pos(-2, 1), Pos(-1, 0)),
      InfectionState(Map(Pos(-1, 0) -> Flagged, Pos(1, 1) -> Infected, Pos(0, 0) -> Weakened, Pos(-1, 1) -> Weakened, Pos(-2, 1) -> Weakened), Pos(-2, 0), Pos(0, -1)),
      InfectionState(Map(Pos(-1, 0) -> Flagged, Pos(1, 1) -> Infected, Pos(0, 0) -> Weakened, Pos(-1, 1) -> Weakened, Pos(-2, 1) -> Weakened, Pos(-2, 0) -> Weakened), Pos(-1, 0), Pos(1, 0)),

      InfectionState(Map(Pos(1, 1) -> Infected, Pos(0, 0) -> Weakened, Pos(-1, 1) -> Weakened, Pos(-2, 1) -> Weakened, Pos(-2, 0) -> Weakened), Pos(-2, 0), Pos(-1, 0)),
      InfectionState(Map(Pos(1, 1) -> Infected, Pos(0, 0) -> Weakened, Pos(-1, 1) -> Weakened, Pos(-2, 1) -> Weakened, Pos(-2, 0) -> Infected), Pos(-3, 0), Pos(-1, 0)),
    )

    val it = iterateBursts(exampleInput)
    forAll (states) { state =>
      assert(it.next() == state)
    }
  }

  test("Part 2 infection bursts") {
    val burstInfections = Table(
      ("bursts", "infections"),
      (100, 26),
      (10000000, 2511944)
    )

    forAll (burstInfections) { (bursts, infections) =>
      assert(Part2.infectionBursts(exampleInput, bursts) == infections)
    }
  }

  test("Part 2 input answer") {
    assert(Part2.infectionBursts(input) == 2511702)
  }
}
