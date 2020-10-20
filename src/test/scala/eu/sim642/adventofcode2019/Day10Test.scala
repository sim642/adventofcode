package eu.sim642.adventofcode2019

import Day10._
import eu.sim642.adventofcodelib.pos.Pos
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite

class Day10Test extends AnyFunSuite with ScalaCheckPropertyChecks {

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

  val exampleInputVaporize =
    """.#....#####...#..
      |##...##.#####..##
      |##...#...#.#####.
      |..#.....X...###..
      |..#.#.....#....##""".stripMargin

  test("Part 1 examples") {
    val inputExpectedPosCount = Table(
      ("input", "expectedPos", "expectedCount"),
      (exampleInput, Pos(3, 4), 8),
      (exampleInput2, Pos(5, 8), 33),
      (exampleInput3, Pos(1, 2), 35),
      (exampleInput4, Pos(6, 3), 41),
      (exampleInput5, Pos(11, 13), 210),
    )

    forAll (inputExpectedPosCount) { (input, expectedPos, expectedCount) =>
      val asteroids = parseAsteroids(input)
      assert(bestMonitoringPosCount(asteroids) == (expectedPos, expectedCount))
    }
  }

  test("Part 1 input answer") {
    assert(bestMonitoringCount(parseAsteroids(input)) == 276)
  }

  test("Part 2 example sequence (small)") {
    val vaporize = vaporizeSeq(Pos(8, 3), parseAsteroids(exampleInputVaporize))

    // first 9
    assert(vaporize(1 - 1) == Pos(8, 1))
    assert(vaporize(2 - 1) == Pos(9, 0))
    assert(vaporize(3 - 1) == Pos(9, 1))
    assert(vaporize(4 - 1) == Pos(10, 0))
    assert(vaporize(5 - 1) == Pos(9, 2))
    assert(vaporize(6 - 1) == Pos(11, 1))
    assert(vaporize(7 - 1) == Pos(12, 1))
    assert(vaporize(8 - 1) == Pos(11, 2))
    assert(vaporize(9 - 1) == Pos(15, 1))
    // second 9
    assert(vaporize(9 + 1 - 1) == Pos(12, 2))
    assert(vaporize(9 + 2 - 1) == Pos(13, 2))
    assert(vaporize(9 + 3 - 1) == Pos(14, 2))
    assert(vaporize(9 + 4 - 1) == Pos(15, 2))
    assert(vaporize(9 + 5 - 1) == Pos(12, 3))
    assert(vaporize(9 + 6 - 1) == Pos(16, 4))
    assert(vaporize(9 + 7 - 1) == Pos(15, 4))
    assert(vaporize(9 + 8 - 1) == Pos(10, 4))
    assert(vaporize(9 + 9 - 1) == Pos(4, 4))
    // third 9
    assert(vaporize(2 * 9 + 1 - 1) == Pos(2, 4))
    assert(vaporize(2 * 9 + 2 - 1) == Pos(2, 3))
    assert(vaporize(2 * 9 + 3 - 1) == Pos(0, 2))
    assert(vaporize(2 * 9 + 4 - 1) == Pos(1, 2))
    assert(vaporize(2 * 9 + 5 - 1) == Pos(0, 1))
    assert(vaporize(2 * 9 + 6 - 1) == Pos(1, 1))
    assert(vaporize(2 * 9 + 7 - 1) == Pos(5, 2))
    assert(vaporize(2 * 9 + 8 - 1) == Pos(1, 0))
    assert(vaporize(2 * 9 + 9 - 1) == Pos(5, 1))
    // fourth 9
    assert(vaporize(3 * 9 + 1 - 1) == Pos(6, 1))
    assert(vaporize(3 * 9 + 2 - 1) == Pos(6, 0))
    assert(vaporize(3 * 9 + 3 - 1) == Pos(7, 0))
    assert(vaporize(3 * 9 + 4 - 1) == Pos(8, 0))
    assert(vaporize(3 * 9 + 5 - 1) == Pos(10, 1))
    assert(vaporize(3 * 9 + 6 - 1) == Pos(14, 0))
    assert(vaporize(3 * 9 + 7 - 1) == Pos(16, 1))
    assert(vaporize(3 * 9 + 8 - 1) == Pos(13, 3))
    assert(vaporize(3 * 9 + 9 - 1) == Pos(14, 3))
  }

  test("Part 2 example sequence (large)") {
    val monitoring = Pos(11, 13)
    val vaporize = vaporizeSeq(monitoring, parseAsteroids(exampleInput5) - monitoring)

    assert(vaporize(1 - 1) == Pos(11, 12))
    assert(vaporize(2 - 1) == Pos(12, 1))
    assert(vaporize(3 - 1) == Pos(12, 2))
    assert(vaporize(10 - 1) == Pos(12, 8))
    assert(vaporize(20 - 1) == Pos(16, 0))
    assert(vaporize(50 - 1) == Pos(16, 9))
    assert(vaporize(100 - 1) == Pos(10, 16))
    assert(vaporize(199 - 1) == Pos(9, 6))
    assert(vaporize(200 - 1) == Pos(8, 2))
    assert(vaporize(201 - 1) == Pos(10, 9))
    assert(vaporize(299 - 1) == Pos(11, 1))
  }

  test("Part 2 example (large)") {
    assert(vaporizeBet(parseAsteroids(exampleInput5)) == 802)
  }

  test("Part 2 input answer") {
    assert(vaporizeBet(parseAsteroids(input)) == 1321)
  }
}
