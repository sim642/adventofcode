package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day7._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day7Test extends FunSuite with ScalaCheckPropertyChecks{

  val exampleInput =
    """123 -> x
      |456 -> y
      |x AND y -> d
      |x OR y -> e
      |x LSHIFT 2 -> f
      |y RSHIFT 2 -> g
      |NOT x -> h
      |NOT y -> i""".stripMargin

  test("parseInstructions") {
    parseInstructions(exampleInput)
    parseInstructions(input)
  }

  test("Part 1 examples") {
    val exampleInstructions = parseInstructions(exampleInput)

    val identExpectedValues = Table(
      ("ident", "expectedValue"),
      ("d", 72),
      ("e", 507),
      ("f", 492),
      ("g", 114),
      ("h", 65412),
      ("i", 65079),
      ("x", 123),
      ("y", 456),
    )

    forAll (identExpectedValues) { (ident, expectedValue) =>
      assert(eval(exampleInstructions)(ident) == expectedValue)
    }
  }

  test("Part 1 input answer") {
    assert(Part1.evalA(input) == 16076)
  }

  test("Part 2 input answer") {
    assert(Part2.evalA(input) == 2797)
  }
}
