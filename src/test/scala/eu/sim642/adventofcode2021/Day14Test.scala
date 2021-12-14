package eu.sim642.adventofcode2021

import Day14._
import eu.sim642.adventofcodelib.IteratorImplicits._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day14Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  val exampleInput =
    """NNCB
      |
      |CH -> B
      |HH -> N
      |CB -> H
      |NH -> C
      |HB -> C
      |HC -> B
      |HN -> C
      |NN -> C
      |BH -> H
      |NC -> B
      |NB -> B
      |BN -> B
      |BB -> N
      |BC -> B
      |CC -> N
      |CN -> C""".stripMargin

  test("Part 1 examples") {
    val Input(initialPolymer, rules) = parseInput(exampleInput)

    val expectedPolymers = Table(
      "expectedPolymer",
      "NNCB",
      "NCNBCHB",
      "NBCCNBBBCBHCB",
      "NBBBCNCCNBBNBNBBCHBHHBCHB",
      "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB",
    )

    val it = iterateRules(initialPolymer, rules)
    forAll(expectedPolymers) { expectedPolymer =>
      assert(it.next() == parsePolymer(expectedPolymer))
    }

    val finalPolymer = iterateRules(initialPolymer, rules)(10)
    assert(finalPolymer.elements == Map(
      'B' -> 1749,
      'C' -> 298,
      'H' -> 161,
      'N' -> 865,
    ))

    assert(elementCountDifference(parseInput(exampleInput)) == 1588)
  }

  test("Part 1 input answer") {
    assert(elementCountDifference(parseInput(input)) == 3009)
  }

  test("Part 2 examples") {
    val Input(initialPolymer, rules) = parseInput(exampleInput)
    val finalPolymer = iterateRules(initialPolymer, rules)(40)
    assert(finalPolymer.elements('B') == 2192039569602L)
    assert(finalPolymer.elements('H') == 3849876073L)

    assert(elementCountDifference(parseInput(exampleInput), 40) == 2188189693529L)
  }

  test("Part 2 input answer") {
    assert(elementCountDifference(parseInput(input), 40) == 3459822539451L)
  }
}
