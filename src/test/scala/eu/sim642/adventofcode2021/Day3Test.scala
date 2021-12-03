package eu.sim642.adventofcode2021

import Day3._
import org.scalatest.funsuite.AnyFunSuite

class Day3Test extends AnyFunSuite {

  val exampleInput =
    """00100
      |11110
      |10110
      |10111
      |10101
      |01111
      |00111
      |11100
      |10000
      |11001
      |00010
      |01010""".stripMargin

  test("Part 1 examples") {
    assert(gammaRate(parseBinaries(exampleInput)) == parseBinary("10110"))
    // TODO: test epsilon, binary2int
    assert(powerConsumption(parseBinaries(exampleInput)) == 198)
  }

  test("Part 1 input answer") {
    assert(powerConsumption(parseBinaries(input)) == 693486)
  }
}
