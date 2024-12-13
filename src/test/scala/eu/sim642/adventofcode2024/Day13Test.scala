package eu.sim642.adventofcode2024

import Day13._
import org.scalatest.funsuite.AnyFunSuite

class Day13Test extends AnyFunSuite {

  val exampleInput =
    """Button A: X+94, Y+34
      |Button B: X+22, Y+67
      |Prize: X=8400, Y=5400
      |
      |Button A: X+26, Y+66
      |Button B: X+67, Y+21
      |Prize: X=12748, Y=12176
      |
      |Button A: X+17, Y+86
      |Button B: X+84, Y+37
      |Prize: X=7870, Y=6450
      |
      |Button A: X+69, Y+23
      |Button B: X+27, Y+71
      |Prize: X=18641, Y=10279""".stripMargin

  test("Part 1 examples") {
    assert(Part1.sumMinWinTokens(parseClawMachines(exampleInput)) == 480)
  }

  test("Part 1 input answer") {
    assert(Part1.sumMinWinTokens(parseClawMachines(input)) == 31623)
  }

  test("Part 2 examples") {
    assert(Part2.sumMinWinTokens(parseClawMachines(exampleInput)) == 875318608908L) // from IRC
  }

  test("Part 2 input answer") {
    assert(Part2.sumMinWinTokens(parseClawMachines(input)) == 93209116744825L)
  }
}
