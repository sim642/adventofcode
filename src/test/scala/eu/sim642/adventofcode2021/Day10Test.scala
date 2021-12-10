package eu.sim642.adventofcode2021

import Day10._
import org.scalatest.funsuite.AnyFunSuite

class Day10Test extends AnyFunSuite {

  val exampleInput =
    """[({(<(())[]>[[{[]{<()<>>
      |[(()[<>])]({[<{<<[]>>(
      |{([(<{}[<>[]}>{[]{[(<()>
      |(((({<>}<{<{<>}{[]{[]{}
      |[[<[([]))<([[{}[[()]]]
      |[{[{({}]{}}([{[{{{}}([]
      |{<[[]]>}<{[{[{[]{()[[[]
      |[<(<(<(<{}))><([]([]()
      |<{([([[(<>()){}]>(<<{{
      |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin

  test("Part 1 examples") {
    assert(parseLine("()") == Legal)
    assert(parseLine("[]") == Legal)
    assert(parseLine("([])") == Legal)
    assert(parseLine("{()()()}") == Legal)
    assert(parseLine("<([{}])>") == Legal)
    assert(parseLine("[<>({}){}[([])<>]]") == Legal)
    assert(parseLine("(((((((((())))))))))") == Legal)

    assert(parseLine("(]").isInstanceOf[Corrupted])
    assert(parseLine("{()()()>").isInstanceOf[Corrupted])
    assert(parseLine("(((()))}").isInstanceOf[Corrupted])
    assert(parseLine("<([]){()}[{}])").isInstanceOf[Corrupted])

    assert(parseLine("{([(<{}[<>[]}>{[]{[(<()>").isInstanceOf[Corrupted])
    assert(parseLine("[[<[([]))<([[{}[[()]]]").isInstanceOf[Corrupted])
    assert(parseLine("[{[{({}]{}}([{[{{{}}([]").isInstanceOf[Corrupted])
    assert(parseLine("[<(<(<(<{}))><([]([]()").isInstanceOf[Corrupted])
    assert(parseLine("<{([([[(<>()){}]>(<<{{").isInstanceOf[Corrupted])

    assert(totalSyntaxErrorScore(parseLines(exampleInput)) == 26397)
  }

  test("Part 1 input answer") {
    assert(totalSyntaxErrorScore(parseLines(input)) == 316851)
  }
}
