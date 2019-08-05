package eu.sim642.adventofcode2016

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import Day14._

class Day14Test extends FunSuite with PropertyChecks {

  val exampleInput = "abc"

  test("md5") {
    assert(md5("The quick brown fox jumps over the lazy dog") == "9e107d9d372bb6826bd81d3542a419d6")
  }

  test("Part 1 keys") {
    val keyIndices = Table("i", 39, 92)

    val it = Part1.keys(exampleInput)
    forAll (keyIndices) { i =>
      assert(it.next()._1 == i)
    }
  }

  test("Part 1 examples") {
    assert(Part1.key64Index(exampleInput) == 22728)
  }

  test("Part 1 input answer") {
    assert(Part1.key64Index(input) == 15168)
  }

  test("Part 2 hash") {
    assert(Part2.hash("abc0") == "a107ff634856bb300138cac6568c0f24")
  }

  test("Part 2 keys") {
    val keyIndices = Table("i", 10)

    val it = Part2.keys(exampleInput)
    forAll (keyIndices) { i =>
      assert(it.next()._1 == i)
    }
  }

  // ignored because very slow (20s each)?

  ignore("Part 2 examples") {
    assert(Part2.key64Index(exampleInput) == 22551)
  }

  ignore("Part 2 input answer") {
    assert(Part2.key64Index(input) == 20864)
  }
}
