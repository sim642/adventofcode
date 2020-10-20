package eu.sim642.adventofcode2018

import Day7._
import org.scalatest.funsuite.AnyFunSuite

class Day7Test extends AnyFunSuite {

  val exampleInput =
    """Step C must be finished before step A can begin.
      |Step C must be finished before step F can begin.
      |Step A must be finished before step B can begin.
      |Step A must be finished before step D can begin.
      |Step B must be finished before step E can begin.
      |Step D must be finished before step E can begin.
      |Step F must be finished before step E can begin.""".stripMargin

  test("Part 1 examples") {
    assert(topologicalSort(parseRequirements(exampleInput)) == "CABDFE")
  }

  test("Part 1 input answer") {
    assert(topologicalSort(parseRequirements(input)) == "MNOUBYITKXZFHQRJDASGCPEVWL")
  }

  test("Part 2 examples") {
    assert(parallelTopologicalSort(parseRequirements(exampleInput), 2, 0) == 15)
  }

  test("Part 2 input answer") {
    assert(parallelTopologicalSort(parseRequirements(input)) == 893)
  }
}
