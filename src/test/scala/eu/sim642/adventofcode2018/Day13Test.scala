package eu.sim642.adventofcode2018

import Day13._
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.StringOps

class Day13Test extends AnyFunSuite {

  // TODO: remove workaround for Scala 3 scalatest stripMargin workaround (https://github.com/scalatest/scalatest/issues/2004)

  val exampleInput1 = new StringOps(
    """|
      !v
      !|
      !|
      !|
      !^
      !|""").stripMargin('!')

  val exampleInput2 = new StringOps(
    """/->-\
      !|   |  /----\
      !| /-+--+-\  |
      !| | |  | v  |
      !\-+-/  \-+--/
      !  \------/   """).stripMargin('!')

  val exampleInput3 = new StringOps(
    """/>-<\
      !|   |
      !| /<+-\
      !| | | v
      !\>+</ |
      !  |   ^
      !  \<->/""").stripMargin('!')

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
