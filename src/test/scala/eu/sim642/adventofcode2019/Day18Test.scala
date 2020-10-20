package eu.sim642.adventofcode2019

import org.scalatest.Suites
import Day18._
import eu.sim642.adventofcode2019.Day18Test._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite

class Day18Test extends Suites(
  new BaseTest,
  new BFSKeyNeighborsSolutionTest,
  new FloydWarshallKeyNeighborsSolutionTest,
)

object Day18Test {

  val exampleInput =
    """#########
      |#b.A.@.a#
      |#########""".stripMargin

  val exampleInput2 =
    """########################
      |#f.D.E.e.C.b.A.@.a.B.c.#
      |######################.#
      |#d.....................#
      |########################""".stripMargin

  val exampleInput3 =
    """########################
      |#...............b.C.D.f#
      |#.######################
      |#.....@.a.B.c.d.A.e.F.g#
      |########################""".stripMargin

  val exampleInput4 =
    """#################
      |#i.G..c...e..H.p#
      |########.########
      |#j.A..b...f..D.o#
      |########@########
      |#k.E..a...g..B.n#
      |########.########
      |#l.F..d...h..C.m#
      |#################""".stripMargin

  val exampleInput5 =
    """########################
      |#@..............ac.GI.b#
      |###d#e#f################
      |###A#B#C################
      |###g#h#i################
      |########################""".stripMargin

  val exampleInputUnsplit =
    """#######
      |#a.#Cd#
      |##...##
      |##.@.##
      |##...##
      |#cB#Ab#
      |#######""".stripMargin

  val exampleInputSplit =
    """#######
      |#a.#Cd#
      |##@#@##
      |#######
      |##@#@##
      |#cB#Ab#
      |#######""".stripMargin

  val exampleInputSplit2 =
    """###############
      |#d.ABC.#.....a#
      |######@#@######
      |###############
      |######@#@######
      |#b.....#.....c#
      |###############""".stripMargin

  val exampleInputSplit3 =
    """#############
      |#DcBa.#.GhKl#
      |#.###@#@#I###
      |#e#d#####j#k#
      |###C#@#@###J#
      |#fEbA.#.FgHi#
      |#############""".stripMargin

  val exampleInputSplit4 =
    """#############
      |#g#f.D#..h#l#
      |#F###e#E###.#
      |#dCba@#@BcIJ#
      |#############
      |#nK.L@#@G...#
      |#M###N#H###.#
      |#o#m..#i#jk.#
      |#############""".stripMargin

  class BaseTest extends AnyFunSuite {
    test("splitEntrance") {
      assert(splitEntrance(parseInput(exampleInputUnsplit)) == parseInput(exampleInputSplit))
    }
  }

  sealed abstract class SolutionTest(solution: Solution) extends AnyFunSuite with ScalaCheckPropertyChecks {

    test("Part 1 examples") {
      val inputExpectedSteps = Table(
        ("input", "expectedSteps"),
        (exampleInput, 8),
        (exampleInput2, 86),
        (exampleInput3, 132),
        (exampleInput4, 136),
        (exampleInput5, 81),
      )

      forAll (inputExpectedSteps) { (input, expectedSteps) =>
        assert(solution.collectKeysSteps(parseInput(input)) == expectedSteps)
      }
    }

    test("Part 1 input answer") {
      assert(solution.collectKeysSteps(parseInput(input)) == 4204)
    }

    test("Part 2 examples (unsplit)") {
      assert(solution.collectKeysStepsSplit(parseInput(exampleInputUnsplit)) == 8)
    }

    test("Part 2 examples (split)") {
      val inputExpectedSteps = Table(
        ("input", "expectedSteps"),
        (exampleInputSplit, 8),
        (exampleInputSplit2, 24),
        (exampleInputSplit3, 32),
        (exampleInputSplit4, 72),
      )

      forAll (inputExpectedSteps) { (input, expectedSteps) =>
        assert(solution.collectKeysSteps(parseInput(input)) == expectedSteps)
      }
    }

    test("Part 2 input answer") {
      assert(solution.collectKeysStepsSplit(parseInput(input)) == 1682)
    }
  }

  class BFSKeyNeighborsSolutionTest extends SolutionTest(BFSKeyNeighborsSolution)

  class FloydWarshallKeyNeighborsSolutionTest extends SolutionTest(FloydWarshallKeyNeighborsSolution)
}
