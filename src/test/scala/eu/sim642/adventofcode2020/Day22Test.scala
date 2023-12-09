package eu.sim642.adventofcode2020

import Day22._
import org.scalatest.funsuite.AnyFunSuite

class Day22Test extends AnyFunSuite {

  val exampleInput =
    """Player 1:
      |9
      |2
      |6
      |3
      |1
      |
      |Player 2:
      |5
      |8
      |4
      |7
      |10""".stripMargin

  lazy val mstksgInput: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day22mstksg.txt")).mkString.trim

  test("Part 1 examples") {
    assert(Part1.winningScore(parseDecks(exampleInput)) == 306)
  }

  test("Part 1 input answer") {
    assert(Part1.winningScore(parseDecks(input)) == 35202)
  }

  test("Part 2 examples") {
    assert(Part2.winningScore(parseDecks(exampleInput)) == 291)
  }

  test("Part 2 input answer") {
    assert(Part2.winningScore(parseDecks(input)) == 32317)
  }

  test("Part 2 mstksg") {
    assert(Part2.winningScore(parseDecks(mstksgInput)) == 32760) // unconfirmed answer
  }
}
