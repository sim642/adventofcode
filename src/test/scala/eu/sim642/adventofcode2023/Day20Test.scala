package eu.sim642.adventofcode2023

import Day20._
import org.scalatest.funsuite.AnyFunSuite

class Day20Test extends AnyFunSuite {

  private val exampleInput =
    """broadcaster -> a, b, c
      |%a -> b
      |%b -> c
      |%c -> inv
      |&inv -> a""".stripMargin

  private val exampleInput2 =
    """broadcaster -> a
      |%a -> inv, con
      |&inv -> b
      |%b -> con
      |&con -> output""".stripMargin

  test("Part 1 examples") {
    assert(countPulses(parseCircuit(exampleInput)) == 32000000)
    assert(countPulses(parseCircuit(exampleInput2)) == 11687500)
  }

  test("Part 1 input answer") {
    assert(countPulses(parseCircuit(input)) == 867118762)
  }
}
