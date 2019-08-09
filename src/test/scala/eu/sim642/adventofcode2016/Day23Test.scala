package eu.sim642.adventofcode2016

import org.scalatest.FunSuite
import Day23._

class Day23Test extends FunSuite {

  val exampleInput =
    """cpy 2 a
      |tgl a
      |tgl a
      |tgl a
      |cpy 1 a
      |dec a
      |dec a""".stripMargin

  test("Part 1 examples") {
    assert(Part1.execRegisterA(exampleInput) == 3)
  }

  test("Part 1 input answer") {
    assert(Part1.execRegisterA(input) == 11893)
  }
}
