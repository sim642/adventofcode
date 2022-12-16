package eu.sim642.adventofcode2022

import Day16._
import org.scalatest.funsuite.AnyFunSuite

class Day16Test extends AnyFunSuite {

  val exampleInput =
    """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
      |Valve BB has flow rate=13; tunnels lead to valves CC, AA
      |Valve CC has flow rate=2; tunnels lead to valves DD, BB
      |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
      |Valve EE has flow rate=3; tunnels lead to valves FF, DD
      |Valve FF has flow rate=0; tunnels lead to valves EE, GG
      |Valve GG has flow rate=0; tunnels lead to valves FF, HH
      |Valve HH has flow rate=22; tunnel leads to valve GG
      |Valve II has flow rate=0; tunnels lead to valves AA, JJ
      |Valve JJ has flow rate=21; tunnel leads to valve II""".stripMargin

  test("Part 1 examples") {
    assert(Part1.mostPressure(parseValves(exampleInput)) == 1651)
  }

  test("Part 1 input answer") {
    assert(Part1.mostPressure(parseValves(input)) == 2265)
  }

  test("Part 2 examples") {
    assert(Part2.mostPressure(parseValves(exampleInput)) == 1707)
  }

  test("Part 2 input answer") { // TODO: optimize? ~7s
    assert(Part2.mostPressure(parseValves(input)) == 2811)
  }
}
