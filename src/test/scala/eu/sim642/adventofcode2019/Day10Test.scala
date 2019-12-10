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

  val exampleInputVaporize =
    """.#....#####...#..
      |##...##.#####..##
      |##...#...#.#####.
      |..#.....X...###..
      |..#.#.....#....##""".stripMargin

  test("isBlocked") {
    assert(isBlocked(Pos(3, 4), Pos(2, 2), Pos(1, 0)))
  }

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

  test("Part 2 examples") {
    // TODO: refactor this test
    println(laserAngle(Pos(8, 3), Pos(8, 1)))
    println(laserAngle(Pos(8, 3), Pos(9, 0)))
    println(laserAngle(Pos(8, 3), Pos(9, 1)))
    println(laserAngle(Pos(8, 3), Pos(10, 0)))
    println(laserAngle(Pos(8, 3), Pos(9, 2)))
    println(laserAngle(Pos(8, 3), Pos(11, 1)))
    println(laserAngle(Pos(8, 3), Pos(12, 1)))
    println(laserAngle(Pos(8, 3), Pos(11, 2)))
    println(laserAngle(Pos(8, 3), Pos(15, 1)))

    val vaporize = vaporizeSeq(Pos(8, 3), parseAsteroids(exampleInputVaporize))
    println(vaporize)
    assert(vaporize(0) == Pos(8, 1))
    assert(vaporize(1) == Pos(9, 0))
    assert(vaporize(2) == Pos(9, 1))
    assert(vaporize(3) == Pos(10, 0))
    assert(vaporize(4) == Pos(9, 2))
    assert(vaporize(5) == Pos(11, 1))
    assert(vaporize(6) == Pos(12, 1))
    assert(vaporize(7) == Pos(11, 2))
    assert(vaporize(8) == Pos(15, 1))

    val vaporize2 = vaporizeSeq(Pos(11, 13), parseAsteroids(exampleInput5) - Pos(11, 13))

    assert(vaporize2(1 - 1) == Pos(11,12))
    assert(vaporize2(2 - 1) == Pos(12,1))
    assert(vaporize2(3 - 1) == Pos(12,2))
    assert(vaporize2(10 - 1) == Pos(12,8))
    assert(vaporize2(20 - 1) == Pos(16,0))
    assert(vaporize2(50 - 1) == Pos(16,9))
    assert(vaporize2(100 - 1) == Pos(10,16))
    assert(vaporize2(199 - 1) == Pos(9,6))
    assert(vaporize2(200 - 1) == Pos(8,2))
    assert(vaporize2(201 - 1) == Pos(10,9))
    assert(vaporize2(299 - 1) == Pos(11,1))

    assert(vaporizeBet(parseAsteroids(exampleInput5)) == 802)
  }

  test("Part 2 input answer") {
    assert(vaporizeBet(parseAsteroids(input)) == 1321)
  }
}
