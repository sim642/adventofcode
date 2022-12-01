package eu.sim642.adventofcode2022

import Day1._
import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends AnyFunSuite {

  val exampleInput =
    """1000
      |2000
      |3000
      |
      |4000
      |
      |5000
      |6000
      |
      |7000
      |8000
      |9000
      |
      |10000""".stripMargin

  test("parseInput") {
    assert(parseElves(exampleInput) == Seq(
      Seq(1000, 2000, 3000),
      Seq(4000),
      Seq(5000, 6000),
      Seq(7000, 8000, 9000),
      Seq(10000),
    ))
  }

  test("Part 1 examples") {
    assert(maxElfTotal(parseElves(exampleInput)) == 24000)
  }

  test("Part 1 input answer") {
    assert(maxElfTotal(parseElves(input)) == 72017)
  }

  test("Part 2 examples") {
    assert(top3ElvesTotal(parseElves(exampleInput)) == 45000)
  }

  test("Part 2 input answer") {
    assert(top3ElvesTotal(parseElves(input)) == 212520)
  }
}
