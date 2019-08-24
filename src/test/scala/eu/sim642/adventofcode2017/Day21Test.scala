package eu.sim642.adventofcode2017

import Day21._
import eu.sim642.adventofcodelib.GridImplicits._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class Day21Test extends FunSuite with PropertyChecks {

  val exampleRules = """../.# => ##./#../...
                       |.#./..#/### => #..#/..../..../#..#""".stripMargin

  test("parsePattern") {
    assert(parsePattern("../.#") == Vector(
      Vector(false, false),
      Vector(false, true)
    ))

    assert(parsePattern(".#./..#/###") == Vector(
      Vector(false, true, false),
      Vector(false, false, true),
      Vector(true, true, true)
    ))

    assert(parsePattern("#..#/..../#..#/.##.") == Vector(
      Vector(true, false, false, true),
      Vector(false, false, false, false),
      Vector(true, false, false, true),
      Vector(false, true, true, false)
    ))
  }

  test("Part 1 example symmetries") {
    val grid = parsePattern(
      """.#.
        |..#
        |###""".stripMargin, '\n')
    val symmetries = grid.symmetries

    assert(symmetries.contains(grid))
    assert(symmetries.contains(parsePattern(
      """.#.
        |#..
        |###""".stripMargin, '\n')))
    assert(symmetries.contains(parsePattern(
      """#..
        |#.#
        |##.""".stripMargin, '\n')))
    assert(symmetries.contains(parsePattern(
      """###
        |..#
        |.#.""".stripMargin, '\n')))
  }

  test("parseRule") {
    assert(parseRule("../.# => ##./#../...") == (parsePattern("../.#") -> parsePattern("##./#../...")))
    assert(parseRule(".#./..#/### => #..#/..../..../#..#") == (parsePattern(".#./..#/###") -> parsePattern("#..#/..../..../#..#")))
  }

  test("groupedGrid") {
    val pattern = parsePattern(
      """#..#
        |....
        |....
        |#..#""".stripMargin, '\n')

    assert(pattern.groupedGrid(2) == Vector(
      Vector(parsePattern("#./.."), parsePattern(".#/..")),
      Vector(parsePattern("../#."), parsePattern("../.#"))
    ))
  }

  test("flattenGrid") {
    val pattern = parsePattern("##./#../...")
    val patternGrid = Vector(
      Vector(pattern, pattern),
      Vector(pattern, pattern)
    )

    assert(patternGrid.flattenGrid == parsePattern("""##.##.
                                                     |#..#..
                                                     |......
                                                     |##.##.
                                                     |#..#..
                                                     |......""".stripMargin, '\n'))
  }

  test("Part 1 example patterns") {
    val patterns = Table(
      "pattern",

      """.#.
        |..#
        |###""".stripMargin,

      """#..#
        |....
        |....
        |#..#""".stripMargin,

      """##.##.
        |#..#..
        |......
        |##.##.
        |#..#..
        |......""".stripMargin
    )

    val it = iteratePattern(parseRules(exampleRules))
    forAll (patterns) { pattern =>
      assert(it.next() == parsePattern(pattern, '\n'))
    }
  }

  test("Part 1 example") {
    assert(countOn(exampleRules, 2) == 12)
  }

  test("Part 1 input answer") {
    assert(countOn(input, 5) == 142)
  }

  test("Part 2 input answer") {
    assert(countOn(input, 18) == 1879071)
  }
}
