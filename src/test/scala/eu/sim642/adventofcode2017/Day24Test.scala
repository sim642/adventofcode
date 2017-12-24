package eu.sim642.adventofcode2017

import Day24._
import org.scalatest.FunSuite

class Day24Test extends FunSuite {

  val exampleInput = """0/2
                       |2/2
                       |2/3
                       |3/4
                       |3/5
                       |0/1
                       |10/1
                       |9/10""".stripMargin

  test("Part 1 example bridges") {
    assert(validBridges(parseComponents(exampleInput)).toSet == Set(
      Seq(Component(0, 1)),
      Seq(Component(0, 1), Component(10, 1)),
      Seq(Component(0, 1), Component(10, 1), Component(9, 10)),
      Seq(Component(0, 2)),
      Seq(Component(0, 2), Component(2, 3)),
      Seq(Component(0, 2), Component(2, 3), Component(3, 4)),
      Seq(Component(0, 2), Component(2, 3), Component(3, 5)),
      Seq(Component(0, 2), Component(2, 2)),
      Seq(Component(0, 2), Component(2, 2), Component(2, 3)),
      Seq(Component(0, 2), Component(2, 2), Component(2, 3), Component(3, 4)),
      Seq(Component(0, 2), Component(2, 2), Component(2, 3), Component(3, 5))
    ))
  }

  test("Part 1 long bridges") {
    assert(validLongBridges(parseComponents(exampleInput)).toSet == Set(
      Seq(Component(0, 1), Component(10, 1), Component(9, 10)),
      Seq(Component(0, 2), Component(2, 3), Component(3, 4)),
      Seq(Component(0, 2), Component(2, 3), Component(3, 5)),
      Seq(Component(0, 2), Component(2, 2), Component(2, 3), Component(3, 4)),
      Seq(Component(0, 2), Component(2, 2), Component(2, 3), Component(3, 5))
    ))
  }

  test("Part 1 example") {
    assert(strongestBridgeStrength(exampleInput) == 31)
  }

  test("Part 1 input answer") {
    assert(strongestBridgeStrength(input) == 2006)
  }

  test("Part 2 longest bridges") {
    assert(validLongestBridges(parseComponents(exampleInput)).toSet == Set(
      Seq(Component(0, 2), Component(2, 2), Component(2, 3), Component(3, 4)),
      Seq(Component(0, 2), Component(2, 2), Component(2, 3), Component(3, 5))
    ))
  }

  test("Part 2 example") {
    assert(longestBridgeStrength(exampleInput) == 19)
  }

  test("Part 2 input answer") {
    assert(longestBridgeStrength(input) == 1994)
  }
}
