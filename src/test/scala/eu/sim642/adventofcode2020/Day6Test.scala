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
    assert(Part1.countYesGroups(parseGroups(exampleInput)) == 6)
    assert(Part1.countYesGroups(parseGroups(exampleInput2)) == 11)
  }

  test("Part 1 input answer") {
    assert(Part1.countYesGroups(parseGroups(input)) == 6630)
  }

  test("Part 2 examples") {
    assert(Part2.countYesGroups(parseGroups(exampleInput2)) == 6)
  }

  test("Part 2 input answer") {
    assert(Part2.countYesGroups(parseGroups(input)) == 3437)
  }
}
