package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day8._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day8Test extends FunSuite with ScalaCheckPropertyChecks {

  val exampleInput =
    """""
      |"abc"
      |"aaa\"aaa"
      |"\x27"""".stripMargin

  test("Part 1 examples") {
    val examples = Table(
      ("s", "expectedLength", "expectedUnescapedLength"),
      ("""""""", 2, 0),
      (""""abc"""", 5, 3),
      (""""aaa\"aaa"""", 10, 7),
      (""""\x27"""", 6, 1),
    )

    forAll (examples) { (s, expectedLength, expectedUnescapedLength) =>
      assert(s.length == expectedLength)
      assert(unescapedLength(s) == expectedUnescapedLength)
    }

    assert(unescapeLengthDiff(exampleInput) == 12)
  }

  test("Part 1 input answer") {
    assert(unescapeLengthDiff(input) == 1350)
  }

  test("Part 2 examples") {
    val examples = Table(
      ("s", "expectedLength", "expectedEscapedLength"),
      ("""""""", 2, 6),
      (""""abc"""", 5, 9),
      (""""aaa\"aaa"""", 10, 16),
      (""""\x27"""", 6, 11),
    )

    forAll (examples) { (s, expectedLength, expectedEscapedLength) =>
      assert(s.length == expectedLength)
      assert(escapedLength(s) == expectedEscapedLength)
    }

    assert(escapeLengthDiff(exampleInput) == 19)
  }

  test("Part 2 input answer") {
    assert(escapeLengthDiff(input) == 2085)
  }
}
