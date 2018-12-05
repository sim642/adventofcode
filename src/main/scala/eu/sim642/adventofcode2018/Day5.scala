package eu.sim642.adventofcode2018

object Day5 {

  def opposites(a: Char, b: Char): Boolean = a != b && a.toLower == b.toLower

  def reactPolymer(s: String): String = {
    /*def helper(init: List[Char], tail: List[Char]): List[Char] = (init, tail) match {
      case (init, Nil) => init.reverse
      case (a :: it, b :: tl) if opposites(a, b) => helper(it, tl)
      case (init, b :: tl) => helper(b :: init, tl)
    }

    helper(Nil, s.toList).mkString("")*/

    s.foldLeft(List[Char]())({
      case (a :: it, b) if opposites(a, b) => it
      case (init, b) => b :: init
    }).reverse.mkString("")
  }

  def reactPolymerLength(s: String): Int = reactPolymer(s).length

  def bestPolymerLength(s: String): Int = {
    val lowerUnits = s.toLowerCase.toSet
    lowerUnits.map(lowerUnit => s.filterNot(c => c == lowerUnit || c == lowerUnit.toUpper)).map(reactPolymerLength).min
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day5.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(reactPolymerLength(input))
    println(bestPolymerLength(input))
  }
}
