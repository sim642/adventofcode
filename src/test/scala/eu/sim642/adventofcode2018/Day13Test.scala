package eu.sim642.adventofcode2018

import Day13._
import org.scalatest.funsuite.AnyFunSuite

class Day13Test extends AnyFunSuite {

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

  val exampleInput3 =
    """/>-<\
      !|   |
      !| /<+-\
      !| | | v
      !\>+</ |
      !  |   ^
      !  \<->/""".stripMargin('!')

  test("Part 1 examples") {
    assert(firstCollisionPos(exampleInput1) == "0,3")
    assert(firstCollisionPos(exampleInput2) == "7,3")
  }

  test("Part 1 input answer") {
    assert(firstCollisionPos(input) == "48,20")
  }

  test("Part 2 examples") {
    assert(lastCartPos(exampleInput3) == "6,4")
  }

  test("Part 2 input answer") {
    assert(lastCartPos(input) == "59,64")
  }
}
