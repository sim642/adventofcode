package eu.sim642.adventofcode2016

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import Day14._

class Day14Test extends FunSuite with PropertyChecks {

  val exampleInput = "abc"

  test("Part 1 keys") {
    val keyIndices = Table("i", 39, 92)

    val it = keys(exampleInput)
    forAll (keyIndices) { i =>
      assert(it.next()._1 == i)
    }
  }

  test("Part 1 examples") {
    assert(key64Index(exampleInput) == 22728)
  }

  test("Part 1 input answer") {
    assert(key64Index(input) == 15168)
  }
}
