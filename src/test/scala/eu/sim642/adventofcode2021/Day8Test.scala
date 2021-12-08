package eu.sim642.adventofcode2021

import Day8._
import Day8Test._
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day8Test extends Suites(
  new Part1Test,
  new NaivePart2SolutionTest,
  new PrecomputePart2SolutionTest,
)

object Day8Test {

  val exampleEntry = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

  val exampleInput =
    """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
      !edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
      !fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
      !fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
      !aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
      !fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
      !dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
      !bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
      !egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
      !gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce""".stripMargin('!')

  class Part1Test extends AnyFunSuite {

    test("Part 1 examples") {
      assert(countUniqueOutputs(parseEntries(exampleInput)) == 26)
    }

    test("Part 1 input answer") {
      assert(countUniqueOutputs(parseEntries(input)) == 387)
    }
  }

  sealed abstract class Part2SolutionTest(part2Solution: Part2Solution) extends AnyFunSuite {

    test("Part 2 examples") {
      assert(part2Solution.decodeEntry(parseEntry(exampleEntry)) == 5353)
      assert(part2Solution.sumDecodeEntries(parseEntries(exampleInput)) == 61229)
    }

    test("Part 2 input answer") {
      assert(part2Solution.sumDecodeEntries(parseEntries(input)) == 986034)
    }
  }

  class NaivePart2SolutionTest extends Part2SolutionTest(NaivePart2Solution)

  class PrecomputePart2SolutionTest extends Part2SolutionTest(PrecomputePart2Solution)
}
