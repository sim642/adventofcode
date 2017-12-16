package eu.sim642.adventofcode2017

import Day16._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class Day16Test extends FunSuite with PropertyChecks {

  test("parseMove") {
    assert(parseMove("s3") == Spin(3))
    assert(parseMove("s1") == Spin(1))

    assert(parseMove("x3/4") == Exchange(3, 4))
    assert(parseMove("pe/b") == Partner('e', 'b'))
  }

  test("applyMove") {
    val moves = Table(
      ("beforePrograms", "move", "afterPrograms"),
      ("abcde", Spin(1), "eabcd"),
      ("eabcd", Exchange(3, 4), "eabdc"),
      ("eabdc", Partner('e', 'b'), "baedc")
    )

    forAll (moves) { (beforePrograms, move, afterPrograms) =>
      assert(applyMove(beforePrograms.toVector, move) == afterPrograms.toVector)
    }
  }

  test("Part 1 example") {
    assert(applyMoves("s1,x3/4,pe/b", "abcde") == "baedc")
  }

  test("Part 1 input answer") {
    assert(applyMoves(input) == "cknmidebghlajpfo")
  }

  test("Part 2 example") {
    assert(applyMovesRepeat("s1,x3/4,pe/b", "abcde", 2) == "ceadb")
  }

  test("Part 2 input answer") {
    assert(applyMovesRepeat(input) == "cbolhmkgfpenidaj")
  }
}
