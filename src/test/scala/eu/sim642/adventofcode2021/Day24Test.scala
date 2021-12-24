package eu.sim642.adventofcode2021

import Day24._
import org.scalatest.funsuite.AnyFunSuite

class Day24Test extends AnyFunSuite {

  lazy val emilyskidsisterInput: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24emilyskidsister.txt")).mkString.trim
  lazy val dphilipsonInput: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24dphilipson.txt")).mkString.trim
  lazy val seligmanInput: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24seligman.txt")).mkString.trim

  test("Part 1 input answer") {
    assert(maxModelNumber(parseSteps(input)) == "97919997299495")
  }

  test("Part 2 input answer") {
    assert(minModelNumber(parseSteps(input)) == "51619131181131")
  }

  test("Part 1 emilyskidsister") {
    assert(maxModelNumber(parseSteps(emilyskidsisterInput)) == "79997391969649")
  }

  test("Part 2 emilyskidsister") {
    assert(minModelNumber(parseSteps(emilyskidsisterInput)) == "16931171414113")
  }

  test("Part 1 dphilipson") {
    assert(maxModelNumber(parseSteps(dphilipsonInput)) == "98491959997994")
  }

  test("Part 2 dphilipson") {
    assert(minModelNumber(parseSteps(dphilipsonInput)) == "61191516111321")
  }

  test("Part 1 seligman") {
    assert(maxModelNumber(parseSteps(seligmanInput)) == "96299896449997")
  }

  test("Part 2 seligman") {
    assert(minModelNumber(parseSteps(seligmanInput)) == "31162141116841")
  }
}
