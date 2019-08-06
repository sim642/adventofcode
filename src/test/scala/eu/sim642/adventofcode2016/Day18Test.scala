package eu.sim642.adventofcode2016

import org.scalatest.FunSuite
import Day18._
import org.scalatest.prop.PropertyChecks

class Day18Test extends FunSuite with PropertyChecks {

  val exampleInput = "..^^."
  val exampleInput2 = ".^^.^.^^^^"

  test("rows") {
    val expectedRows = Table(
      "expectedRow",
      "..^^.",
      ".^^^^",
      "^^..^",
    )

    val it = rows(exampleInput)
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

    val it = rows(exampleInput2)
    forAll (expectedRows) { expectedRow =>
      assert(it.next() == expectedRow)
    }
  }

  test("Part 1 examples") {
    assert(countSafe(exampleInput2, 10) == 38)
  }

  test("Part 1 input answer") {
    assert(countSafe(input) == 1989)
  }
}
