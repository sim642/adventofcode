package eu.sim642.adventofcode2022

import Day20.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day20Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  val exampleInput =
    """1
      |2
      |-3
      |3
      |-2
      |0
      |4""".stripMargin

  test("mix") {
    //assert(mixOne(Vector(4, 5, 6, 1, 7, 8, 9), 1) == Vector(4, 5, 6, 7, 1, 8, 9))
    //assert(mixOne(Vector(4, -2, 5, 6, 7, 8, 9), -2) == Vector(4, 5, 6, 7, 8, -2, 9))
  }

  test("Part 1 examples") {
    val exampleFile = parseFile(exampleInput)

    val arrangements = Table(
      "arrangement",
      Seq(1, 2, -3, 3, -2, 0, 4),
      Seq(2, 1, -3, 3, -2, 0, 4),
      Seq(1, -3, 2, 3, -2, 0, 4),
      Seq(1, 2, 3, -2, -3, 0, 4),
      Seq(1, 2, -2, -3, 0, 3, 4),
      Seq(1, 2, -3, 0, 3, 4, -2),
      Seq(1, 2, -3, 0, 3, 4, -2),
      Seq(1, 2, -3, 4, 0, 3, -2),
    )

    val it = iterateMix(exampleFile)
    forAll (arrangements) { arrangement =>
      assert(it.next() == arrangement)
    }

    assert(mix(exampleFile) == Vector(1, 2, -3, 4, 0, 3, -2))
    assert(mixGroveCoordinates(exampleFile) == 3)
  }

  test("Part 1 input answer") {
    assert(mixGroveCoordinates(parseFile(input)) == 4224) // TODO: optimize
  }
}
