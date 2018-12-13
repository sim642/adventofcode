package eu.sim642.adventofcode2018

import org.scalatest.FunSuite
import Day13._

class Day13Test extends FunSuite {

  val exampleInput1 =
    """|
      !v
      !|
      !|
      !|
      !^
      !|""".stripMargin('!')

  val exampleInput2 =
    """/->-\
      !|   |  /----\
      !| /-+--+-\  |
      !| | |  | v  |
      !\-+-/  \-+--/
      !  \------/   """.stripMargin('!')

  test("Part 1 examples") {
    assert(firstCollisionPos(exampleInput1) == "0,3")
    assert(firstCollisionPos(exampleInput2) == "7,3")
  }

  test("Part 1 input answer") {
    assert(firstCollisionPos(input) == "48,20")
  }
}
