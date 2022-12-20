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

  /*test("mix") {
    //assert(mixOne(Vector(4, 5, 6, 1, 7, 8, 9), 1) == Vector(4, 5, 6, 7, 1, 8, 9))
    //assert(mixOne(Vector(4, -2, 5, 6, 7, 8, 9), -2) == Vector(4, 5, 6, 7, 8, -2, 9))
  }*/

  test("Part 1 examples") {
    val exampleFile = parseFile(exampleInput)

    /*val arrangements = Table(
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

    val it = Part1.iterateMix(exampleFile)
    forAll (arrangements) { arrangement =>
      assert(it.next() == arrangement)
    }

    assert(Part1.mix(exampleFile) == Vector(1, 2, -3, 4, 0, 3, -2))*/
    assert(Part1.mixGroveCoordinates(exampleFile) == 3)
  }

  test("Part 1 input answer") {
    assert(Part1.mixGroveCoordinates(parseFile(input)) == 4224) // TODO: optimize
  }

  test("Part 2 examples") {
    val exampleFile = parseFile(exampleInput)

    /*val arrangements = Table(
      "arrangement",
      Seq(811589153, 1623178306, -2434767459L, 2434767459L, -1623178306, 0, 3246356612L),
      Seq(0, -2434767459L, 3246356612L, -1623178306, 2434767459L, 1623178306, 811589153),
      Seq(0, 2434767459L, 1623178306, 3246356612L, -2434767459L, -1623178306, 811589153),
      Seq(0, 811589153, 2434767459L, 3246356612L, 1623178306, -1623178306, -2434767459L),
      Seq(0, 1623178306, -2434767459L, 811589153, 2434767459L, 3246356612L, -1623178306),
      Seq(0, 811589153, -1623178306, 1623178306, -2434767459L, 3246356612L, 2434767459L),
      Seq(0, 811589153, -1623178306, 3246356612L, -2434767459L, 1623178306, 2434767459L),
      Seq(0, -2434767459L, 2434767459L, 1623178306, -1623178306, 811589153, 324635661),
      Seq(0, 1623178306, 3246356612L, 811589153, -2434767459L, 2434767459L, -1623178306),
      Seq(0, 811589153, 1623178306, -2434767459L, 3246356612L, 2434767459L, -1623178306),
      Seq(0, -2434767459L, 1623178306, 3246356612L, -1623178306, 2434767459L, 811589153),
    )*/

    //val it = Part2.iterateMixRounds(exampleFile)
    //forAll (arrangements) { arrangement =>
    //  assert(it.next() == arrangement)
    //}
    //
    //assert(Part2.mixRounds(exampleFile) == Vector(0, -2434767459L, 1623178306, 3246356612L, -1623178306, 2434767459L, 811589153))
    assert(Part2.mixGroveCoordinates(exampleFile) == 1623178306)
  }

  test("Part 2 input answer") {
    assert(Part2.mixGroveCoordinates(parseFile(input)) == 861907680486L) // TODO: optimize
  }
}
