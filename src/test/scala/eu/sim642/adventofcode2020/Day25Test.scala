package eu.sim642.adventofcode2020

import Day25._
import org.scalatest.funsuite.AnyFunSuite

class Day25Test extends AnyFunSuite {

  val exampleInput =
    """5764801
      |17807724""".stripMargin

  test("Part 1 examples") {
    assert(findLoopSize(5764801) == 8)
    assert(findLoopSize(17807724) == 11)

    assert(findEncryptionKey(parsePublicKeys(exampleInput)) == 14897079)
  }

  test("Part 1 input answer") {
    assert(findEncryptionKey(parsePublicKeys(input)) == 297257)
  }
}
