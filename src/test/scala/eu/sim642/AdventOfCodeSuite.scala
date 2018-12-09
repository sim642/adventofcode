package eu.sim642

import org.scalatest.Suite

trait AdventOfCodeSuite { thisSuite: Suite =>
  private val classNameRegex = """eu\.sim642\.adventofcode(\d+)\.Day(\d+)Test""".r

  override def suiteName: String = thisSuite.getClass.getName match {
    case classNameRegex(year, day) => s"$year day $day"
  }
}
