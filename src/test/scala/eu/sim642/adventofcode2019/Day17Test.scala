package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day17._
import intcode.parseProgram
import eu.sim642.adventofcodelib.IteratorImplicits._

class Day17Test extends FunSuite {

  val exampleGrid =
    """..#..........
      |..#..........
      |#######...###
      |#.#...#...#.#
      |#############
      |..#...#...#..
      |..#####...^..""".stripMargin

  val exampleGrid2 =
    """#######...#####
      |#.....#...#...#
      |#.....#...#...#
      |......#...#...#
      |......#...###.#
      |......#.....#.#
      |^########...#.#
      |......#.#...#.#
      |......#########
      |........#...#..
      |....#########..
      |....#...#......
      |....#...#......
      |....#...#......
      |....#####......""".stripMargin

  test("Part 1 examples") {
    assert(sumAlignmentParameters(parseGrid(exampleGrid)) == 76)
  }

  test("Part 1 input answer") {
    assert(sumAlignmentParameters(parseProgram(input)) == 3660)
  }

  ignore("Part 2 examples") {
    // internal logic of dustCollected
    // TODO: fix this test, currently fails because even a trivial eager factoring works

    val path = getPath(parseGrid(exampleGrid2))
    assert(pathToString(path) == "R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2")

    val pathParts = factorPathParts(Seq(path)).head
    assert(pathToString(pathParts(0)) == "R,8,R,8")
    assert(pathToString(pathParts(1)) == "R,4,R,4,R,8")
    assert(pathToString(pathParts(2)) == "L,6,L,2")

    val mainPath = reconstructMainPaths(path, pathParts).head
    assert(mainPathToString(mainPath) == "A,B,C,B,A,C")
  }

  test("Part 2 input answer") {
    assert(dustCollected(parseProgram(input)) == 962913)
  }
}
