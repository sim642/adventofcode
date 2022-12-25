package eu.sim642.adventofcode2022

import Day25._
import org.scalatest.funsuite.AnyFunSuite

class Day25Test extends AnyFunSuite {

  val exampleInput =
    """1=-0-2
      |12111
      |2=0=
      |21
      |2=01
      |111
      |20012
      |112
      |1=-1=
      |1-12
      |12
      |1=
      |122""".stripMargin

  test("Part 1 examples") {
    assert(sumSnafus(parseSnafus(exampleInput)) == "2=-1=0")
  }

  test("Part 1 input answer") {
    assert(sumSnafus(parseSnafus(input)) == "2-==10===-12=2-1=-=0")
  }
}
