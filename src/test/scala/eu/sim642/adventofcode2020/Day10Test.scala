package eu.sim642.adventofcode2020

import Day10._
import org.scalatest.funsuite.AnyFunSuite

class Day10Test extends AnyFunSuite {

  val exampleInput =
    """16
      |10
      |15
      |5
      |1
      |11
      |7
      |19
      |6
      |12
      |4""".stripMargin

  val exampleInput2 =
    """28
      |33
      |18
      |42
      |31
      |14
      |46
      |20
      |48
      |47
      |24
      |23
      |49
      |45
      |19
      |38
      |39
      |11
      |1
      |32
      |25
      |35
      |8
      |17
      |7
      |9
      |4
      |2
      |34
      |10
      |3""".stripMargin

  test("Part 1 examples") {
    assert(differencesProductAll(parseJolts(exampleInput)) == 7 * 5)
    assert(differencesProductAll(parseJolts(exampleInput2)) == 22 * 10)
  }

  test("Part 1 input answer") {
    assert(differencesProductAll(parseJolts(input)) == 2070)
  }
}
