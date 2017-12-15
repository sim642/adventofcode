package eu.sim642.adventofcode2017

import Day15._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class Day15Test extends FunSuite with PropertyChecks {

  test("Part 1 example generators") {
    val genA = new GeneratorA(65)
    val genB = new GeneratorB(8921)

    val values = Table(
      ("a", "b"),
      (1092455, 430625591),
      (1181022009, 1233683848),
      (245556042, 1431495498),
      (1744312007, 137874439),
      (1352636452, 285222916)
    )

    forAll (values) { (a, b) =>
      assert(genA.next() == a)
      assert(genB.next() == b)
    }
  }

  test("Part 1 example matches count") {
    assert(matchesCount(65, 8921, 5) == 1)
    assert(matchesCount(65, 8921) == 588)
  }

  test("Part 1 input answer") {
    assert(matchesCount(inputStartA, inputStartB) == 569)
  }

  test("Part 2 example generators") {
    val genA = new MultipleGeneratorA(65)
    val genB = new MultipleGeneratorB(8921)

    val values = Table(
      ("a", "b"),
      (1352636452, 1233683848),
      (1992081072, 862516352),
      (530830436, 1159784568),
      (1980017072, 1616057672),
      (740335192, 412269392)
    )

    forAll (values) { (a, b) =>
      assert(genA.next() == a)
      assert(genB.next() == b)
    }
  }

  test("Part 2 example matches count") {
    assert(multipleMatchesCount(65, 8921, 1055) == 0)
    assert(multipleMatchesCount(65, 8921, 1056) == 1)
    assert(multipleMatchesCount(65, 8921) == 309)
  }

  test("Part 2 input answer") {
    assert(multipleMatchesCount(inputStartA, inputStartB) == 298)
  }
}
