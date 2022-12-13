package eu.sim642.adventofcode2022

import Day13._
import org.scalatest.funsuite.AnyFunSuite

class Day13Test extends AnyFunSuite {

  val exampleInput =
    """[1,1,3,1,1]
      |[1,1,5,1,1]
      |
      |[[1],[2,3,4]]
      |[[1],4]
      |
      |[9]
      |[[8,7,6]]
      |
      |[[4,4],4,4]
      |[[4,4],4,4,4]
      |
      |[7,7,7,7]
      |[7,7,7]
      |
      |[]
      |[3]
      |
      |[[[]]]
      |[[]]
      |
      |[1,[2,[3,[4,[5,6,7]]]],8,9]
      |[1,[2,[3,[4,[5,6,0]]]],8,9]""".stripMargin

  test("Part 1 examples") {
    assert(sumPairOrderedIndices(parsePackets(exampleInput)) == 13)
  }

  test("Part 1 input answer") {
    assert(sumPairOrderedIndices(parsePackets(input)) == 5808)
  }

  test("Part 2 examples") {
    assert(decoderKey(parsePackets(exampleInput)) == 140)
  }

  test("Part 2 input answer") {
    assert(decoderKey(parsePackets(input)) == 22713)
  }
}
