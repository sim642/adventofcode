package eu.sim642.adventofcode2018

object Day5 {

  def opposites(a: Char, b: Char): Boolean = (a.isLower, b.isLower) match {
    case (true, false) | (false, true) => a.toLower == b.toLower
    case _ => false
  }

  def reactPolymer(s: String): String = {
    def helper(polymer: Vector[Char], i: Int): Vector[Char] = {
      if (i >= polymer.length - 1)
        polymer
      else if (opposites(polymer(i), polymer(i + 1))) {
        val (before, after) = polymer.splitAt(i)
        helper(before ++ after.drop(2), math.max(0, i - 1))
      }
      else
        helper(polymer, i + 1)
    }

    helper(s.toVector, 0).mkString("")
  }

  def reactPolymerLength(s: String): Int = reactPolymer(s).length


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day5.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(reactPolymerLength(input))
  }
}
