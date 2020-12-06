package eu.sim642.adventofcode2020

import Day6._
import org.scalatest.funsuite.AnyFunSuite

class Day6Test extends AnyFunSuite {

  val exampleInput =
    """abcx
      |abcy
      |abcz""".stripMargin

  val exampleInput2 =
    """abc
      |
      |a
      |b
      |c
      |
      |ab
      |ac
      |
      |a
      |a
      |a
      |a
      |
      |b""".stripMargin

  test("Part 1 examples") {
    assert(countYesGroups(parseGroups(exampleInput)) == 6)
    assert(countYesGroups(parseGroups(exampleInput2)) == 11)
  }

  test("Part 1 input answer") {
    assert(countYesGroups(parseGroups(input)) == 6630)
  }
}
