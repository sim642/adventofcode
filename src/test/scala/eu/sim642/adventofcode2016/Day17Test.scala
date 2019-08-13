package eu.sim642.adventofcode2016

import org.scalatest.FunSuite
import Day17._
import eu.sim642.adventofcodelib.pos.Pos
import org.scalatest.prop.PropertyChecks

class Day17Test extends FunSuite with PropertyChecks {

  val exampleInput = "hijkl"

  test("VaultPos moves") {
    val vaultPosMoves = Table(
      ("vaultPos", "expectedMoves"),
      (VaultPos(exampleInput, Pos(0, 0), ""), Seq(VaultPos(exampleInput, Pos(0, 1), "D"))),
      (VaultPos(exampleInput, Pos(0, 1), "D"), Seq(VaultPos(exampleInput, Pos(0, 0), "DU"), VaultPos(exampleInput, Pos(1, 1), "DR"))),
      (VaultPos(exampleInput, Pos(1, 1), "DR"), Seq()),
      (VaultPos(exampleInput, Pos(0, 0), "DU"), Seq(VaultPos(exampleInput, Pos(1, 0), "DUR"))),
      (VaultPos(exampleInput, Pos(1, 0), "DUR"), Seq()),
    )

    forAll (vaultPosMoves) { (vaultPos, expectedMoves) =>
      assert(vaultPos.moves == expectedMoves)
    }
  }

  test("Part 1 examples") {
    val passcodeShortestPaths = Table(
      ("passcode", "expectedShortestPath"),
      ("ihgpwlah", "DDRRRD"),
      ("kglvqrro", "DDUDRLRRUDRD"),
      ("ulqzkmiv", "DRURDRUDDLLDLUURRDULRLDUUDDDRR"),
    )

    forAll (passcodeShortestPaths) { (passcode, expectedShortestPath) =>
      assert(shortestVaultPath(passcode) == expectedShortestPath)
    }
  }

  test("Part 1 input answer") {
    assert(shortestVaultPath(input) == "RLDRUDRDDR")
  }

  test("Part 2 examples") {
    val passcodeLengths = Table(
      ("passcode", "expectedLength"),
      ("ihgpwlah", 370),
      ("kglvqrro", 492),
      ("ulqzkmiv", 830),
    )

    forAll (passcodeLengths) { (passcode, expectedLength) =>
      assert(longestVaultPathLength(passcode) == expectedLength)
    }
  }

  test("Part 2 input answer") {
    assert(longestVaultPathLength(input) == 498)
  }
}
