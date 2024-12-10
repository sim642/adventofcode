package eu.sim642.adventofcode2024

import Day10._
import org.scalatest.funsuite.AnyFunSuite

class Day10Test extends AnyFunSuite {

  val exampleInput =
    """0123
      |1234
      |8765
      |9876""".stripMargin

  val exampleInput2 =
    """89010123
      |78121874
      |87430965
      |96549874
      |45678903
      |32019012
      |01329801
      |10456732""".stripMargin

  val exampleTrailhead =
    """...0...
      |...1...
      |...2...
      |6543456
      |7.....7
      |8.....8
      |9.....9""".stripMargin

  val exampleTrailhead2 =
    """..90..9
      |...1.98
      |...2..7
      |6543456
      |765.987
      |876....
      |987....""".stripMargin

  val exampleTrailheads =
    """10..9..
      |2...8..
      |3...7..
      |4567654
      |...8..3
      |...9..2
      |.....01""".stripMargin

  val exampleTrailhead3 =
    """.....0.
      |..4321.
      |..5..2.
      |..6543.
      |..7..4.
      |..8765.
      |..9....""".stripMargin

  val exampleTrailhead4 =
    """..90..9
      |...1.98
      |...2..7
      |6543456
      |765.987
      |876....
      |987....""".stripMargin

  val exampleTrailhead5 =
    """012345
      |123456
      |234567
      |345678
      |4.6789
      |56789.""".stripMargin

  test("Part 1 examples") {
    assert(sumTrailheadScores(parseGrid(exampleInput)) == 1)
    assert(sumTrailheadScores(parseGrid(exampleTrailhead)) == 2)
    assert(sumTrailheadScores(parseGrid(exampleTrailhead2)) == 4)
    assert(sumTrailheadScores(parseGrid(exampleTrailheads)) == 3)
    assert(sumTrailheadScores(parseGrid(exampleInput2)) == 36)
  }

  test("Part 1 input answer") {
    assert(sumTrailheadScores(parseGrid(input)) == 557)
  }

  test("Part 2 examples") {
    assert(sumTrailheadRatings(parseGrid(exampleInput)) == 16) // not in text
    assert(sumTrailheadRatings(parseGrid(exampleTrailhead3)) == 3)
    assert(sumTrailheadRatings(parseGrid(exampleTrailhead4)) == 13)
    assert(sumTrailheadRatings(parseGrid(exampleTrailhead5)) == 227)
    assert(sumTrailheadRatings(parseGrid(exampleInput2)) == 81)
  }

  test("Part 2 input answer") {
    assert(sumTrailheadRatings(parseGrid(input)) == 1062)
  }
}
