package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day10._
import eu.sim642.adventofcodelib.pos.Pos
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day10Test extends FunSuite with ScalaCheckPropertyChecks {

  val exampleInput =
    """.#..#
      |.....
      |#####
      |....#
      |...##""".stripMargin

  val exampleInput2 =
    """......#.#.
      |#..#.#....
      |..#######.
      |.#.#.###..
      |.#..#.....
      |..#....#.#
      |#..#....#.
      |.##.#..###
      |##...#..#.
      |.#....####""".stripMargin

  val exampleInput3 =
    """#.#...#.#.
      |.###....#.
      |.#....#...
      |##.#.#.#.#
      |....#.#.#.
      |.##..###.#
      |..#...##..
      |..##....##
      |......#...
      |.####.###.""".stripMargin

  val exampleInput4 =
    """.#..#..###
      |####.###.#
      |....###.#.
      |..###.##.#
      |##.##.#.#.
      |....###..#
      |..#.#..#.#
      |#..#.#.###
      |.##...##.#
      |.....#.#..""".stripMargin

  val exampleInput5 =
    """.#..##.###...#######
      |##.############..##.
      |.#.######.########.#
      |.###.#######.####.#.
      |#####.##.#.##.###.##
      |..#####..#.#########
      |####################
      |#.####....###.#.#.##
      |##.#################
      |#####.##.###..####..
      |..######..##.#######
      |####.##.####...##..#
      |.#####..#.######.###
      |##...#.##########...
      |#.##########.#######
      |.####.#.###.###.#.##
      |....##.##.###..#####
      |.#.#.###########.###
      |#.#.#.#####.####.###
      |###.##.####.##.#..##""".stripMargin

  test("isBlocked") {
    assert(isBlocked(Pos(3, 4), Pos(2, 2), Pos(1, 0)))
  }

  test("Part 1 examples") {
    val inputExpectedPosCount = Table(
      ("input", "expectedPos", "expectedCount"),
      (exampleInput, Pos(3, 4), 8)
    )

    forAll (inputExpectedPosCount) { (input, expectedPos, expectedCount) =>
      val asteroids = parseAsteroids(input)
      assert(bestMonitoringPosCount(asteroids) == (expectedPos, expectedCount))
    }
  }

  test("Part 1 input answer") {
    assert(bestMonitoringCount(parseAsteroids(input)) == 276)
  }
}
