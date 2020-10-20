package eu.sim642.adventofcode2016

import Day18._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite

class Day18Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  val exampleInput = "..^^."
  val exampleInput2 = ".^^.^.^^^^"

  test("rows") {
    val expectedRows = Table(
      "expectedRow",
      "..^^.",
      ".^^^^",
      "^^..^",
    )

    val it = rowStrings(exampleInput)
    forAll (expectedRows) { expectedRow =>
      assert(it.next() == expectedRow)
    }
  }

  test("rows (larger)") {
    val expectedRows = Table(
      "expectedRow",
      ".^^.^.^^^^",
      "^^^...^..^",
      "^.^^.^.^^.",
      "..^^...^^^",
      ".^^^^.^^.^",
      "^^..^.^^..",
      "^^^^..^^^.",
      "^..^^^^.^^",
      ".^^^..^.^^",
      "^^.^^^..^^",
    )

    val it = rowStrings(exampleInput2)
    forAll (expectedRows) { expectedRow =>
      assert(it.next() == expectedRow)
    }
  }

  test("Part 1 examples") {
    assert(Part1.countSafe(exampleInput2, 10) == 38)
  }

  test("Part 1 input answer") {
    assert(Part1.countSafe(input) == 1989)
  }

  test("Part 2 input answer") {
    assert(Part2.countSafe(input) == 19999894)
  }
}
