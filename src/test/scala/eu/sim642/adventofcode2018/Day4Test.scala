package eu.sim642.adventofcode2018

import org.scalatest.FunSuite
import Day4._
import eu.sim642.AdventOfCodeSuite

class Day4Test extends FunSuite with AdventOfCodeSuite {

  val exampleInput =
    """[1518-11-01 00:00] Guard #10 begins shift
      |[1518-11-01 00:05] falls asleep
      |[1518-11-01 00:25] wakes up
      |[1518-11-01 00:30] falls asleep
      |[1518-11-01 00:55] wakes up
      |[1518-11-01 23:58] Guard #99 begins shift
      |[1518-11-02 00:40] falls asleep
      |[1518-11-02 00:50] wakes up
      |[1518-11-03 00:05] Guard #10 begins shift
      |[1518-11-03 00:24] falls asleep
      |[1518-11-03 00:29] wakes up
      |[1518-11-04 00:02] Guard #99 begins shift
      |[1518-11-04 00:36] falls asleep
      |[1518-11-04 00:46] wakes up
      |[1518-11-05 00:03] Guard #99 begins shift
      |[1518-11-05 00:45] falls asleep
      |[1518-11-05 00:55] wakes up""".stripMargin

  test("parseRecord") {
    assert(parseRecord("[1518-11-01 00:00] Guard #10 begins shift") == Record("1518-11-01 00:00", BeginShift(10)))
    assert(parseRecord("[1518-11-01 00:05] falls asleep") == Record("1518-11-01 00:05", FallAsleep))
    assert(parseRecord("[1518-11-01 00:25] wakes up") == Record("1518-11-01 00:25", WakeUp))
  }

  test("parseShifts") {
    assert(parseShifts(parseRecords(exampleInput)) == List(
      Shift(10, (5 until 25).toSet ++ (30 until 55).toSet),
      Shift(99, (40 until 50).toSet),
      Shift(10, (24 until 29).toSet),
      Shift(99, (36 until 46).toSet),
      Shift(99, (45 until 55).toSet),
    ))
  }

  test("Part 1 examples") {
    assert(Strategy1.choose(exampleInput) == 240)
  }

  test("Part 1 input answer") {
    assert(Strategy1.choose(input) == 103720)
  }

  test("Part 2 examples") {
    assert(Strategy2.choose(exampleInput) == 4455)
  }

  test("Part 2 input answer") {
    assert(Strategy2.choose(input) == 110913)
  }
}
