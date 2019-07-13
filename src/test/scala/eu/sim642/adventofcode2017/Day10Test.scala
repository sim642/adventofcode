package eu.sim642.adventofcode2017

import Day10._
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day10Test extends FunSuite with ScalaCheckPropertyChecks {

  val exampleInitialState = KnotState(Vector(0, 1, 2, 3, 4))

  test("Part 1 example states") {
    val lengthStates = Table(
      ("length", "state"),
      (3, KnotState(Vector(2, 1, 0, 3, 4), 3, 1)),
      (4, KnotState(Vector(4, 3, 0, 1, 2), 3, 2)),
      (1, KnotState(Vector(4, 3, 0, 1, 2), 1, 3)),
      (5, KnotState(Vector(3, 4, 2, 1, 0), 4, 4))
    )

    var currentState = exampleInitialState
    forAll (lengthStates) { (length, state) =>
      currentState = currentState.reversed(length)
      assert(currentState == state)
    }
  }

  test("Part 1 example check product") {
    assert(simulate(exampleInitialState, Seq(3, 4, 1, 5)).checkProduct == 12)
  }

  test("Part 1 input answer") {
    assert(knotCheckProduct(input) == 46600)
  }

  test("Part 2 example lengths") {
    assert(asciiLengths("1,2,3") == Seq(49,44,50,44,51,17,31,73,47,23))
  }

  test("Part 2 example dense hash") {
    assert(sparse2dense(Seq(65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22)) == Seq(64))
  }

  test("Part 2 example hex string") {
    assert(mkHexString(Seq(64, 7, 255)) == "4007ff")
  }

  test("Part 2 examples") {
    val hashes = Table(
      ("input", "hash"),
      ("", "a2582a3a0e66e6e86e3812dcb672a272"),
      ("AoC 2017", "33efeb34ea91902bb2f59c9920caa6cd"),
      ("1,2,3", "3efbe78a8d82f29979031a4aa0b16a9d"),
      ("1,2,4", "63960835bcdc130f0b66d7ff4f6a5a8e")
    )

    forAll (hashes) { (input, hash) =>
      assert(knotHashHex(input) == hash)
    }
  }
}
