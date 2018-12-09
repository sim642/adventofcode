package eu.sim642.adventofcode2017

import Day25._
import eu.sim642.AdventOfCodeSuite
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class Day25Test extends FunSuite with PropertyChecks with AdventOfCodeSuite {

  lazy val exampleInput: String = io.Source.fromInputStream(getClass.getResourceAsStream("day25example.txt")).mkString.trim

  test("parseConfiguration") {
    assert(parseConfiguration(exampleInput) == Configuration(
      Map(
        ('A', 0) -> ('B', 1, Right),
        ('A', 1) -> ('B', 0, Left),
        ('B', 0) -> ('A', 1, Left),
        ('B', 1) -> ('A', 1, Right)
      ),
      'A'
    ))
  }

  test("Part 1 example states") {
    val initialConfiguration = parseConfiguration(exampleInput)

    val states = Table(
      ("state", "cursor", "tape"),
      ('A', 0, Map.empty),
      ('B', 1, Map(0 -> 1)),
      ('A', 0, Map(0 -> 1, 1 -> 1)),
      ('B', -1, Map(0 -> 0, 1 -> 1)),
      ('A', -2, Map(-1 -> 1, 0 -> 0, 1 -> 1)),
      ('B', -1, Map(-2 -> 1, -1 -> 1, 0 -> 0, 1 -> 1)),
      ('A', 0, Map(-2 -> 1, -1 -> 1, 0 -> 0, 1 -> 1))
    )

    val it = iterateTuringMachine(initialConfiguration)
    forAll (states) { (state, cursor, tape) =>
      val configuration = it.next()

      assert(configuration.state == state)
      assert(configuration.cursor == cursor)
      assert(configuration.tape == tape)
    }
  }

  test("Part 1 example") {
    assert(diagnosticChecksum(exampleInput) == 3)
  }

  test("Part 1 input answer") {
    assert(diagnosticChecksum(input) == 4769)
  }
}
