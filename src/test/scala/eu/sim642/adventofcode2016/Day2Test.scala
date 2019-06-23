package eu.sim642.adventofcode2016

import Day2._
import org.scalatest.FunSuite

class Day2Test extends FunSuite {

  test("Part 1 examples") {
    assert(bathroomCode("""ULL
                          |RRDDD
                          |LURDL
                          |UUUUD""".stripMargin) == "1985")
  }

  test("Part 1 input answer") {
    assert(bathroomCode(input) == "92435")
  }
}
