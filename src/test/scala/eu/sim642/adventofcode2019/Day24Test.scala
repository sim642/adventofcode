package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day24._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day24Test extends FunSuite with ScalaCheckPropertyChecks {

  val exampleInput =
    """....#
      |#..#.
      |#..##
      |..#..
      |#....""".stripMargin

  test("Part 1 examples") {
    val expectedGrid = Table(
      "expectedGrid",
      exampleInput, // trivial
      """#..#.
        |####.
        |###.#
        |##.##
        |.##..""".stripMargin,
      """#####
        |....#
        |....#
        |...#.
        |#.###""".stripMargin,
      """#....
        |####.
        |...##
        |#.##.
        |.##.#""".stripMargin,
      """####.
        |....#
        |##..#
        |.....
        |##...""".stripMargin,
    )

    val it = Iterator.iterate(parseGrid(exampleInput))(step)
    forAll (expectedGrid) { expectedGrid =>
      assert(it.next() == parseGrid(expectedGrid))
    }

    assert(findCycleGrid(parseGrid(exampleInput)) ==
      parseGrid(""".....
        |.....
        |.....
        |#....
        |.#...""".stripMargin))

    assert(findCycleBiodiversityRating(parseGrid(exampleInput)) == 2129920)
  }

  test("Part 1 input answer") {
    assert(findCycleBiodiversityRating(parseGrid(input)) == 18859569)
  }
}
