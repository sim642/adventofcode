package eu.sim642.adventofcode2019

object Day6 {

  def getChildrenMap(parentMap: Map[String, String]): Map[String, Set[String]] = {
    parentMap.groupBy(_._2).view.mapValues(_.keySet).toMap.withDefaultValue(Set.empty)
  }

  def countOrbits(parentMap: Map[String, String]): Int = {
    val childrenMap = getChildrenMap(parentMap)

    def helper(obj: String, depth: Int): Int = {
      depth + childrenMap(obj).view.map(helper(_, depth + 1)).sum
    }

    helper("COM", 0)
  }

  def parseParentMap(input: String): Map[String, String] = {
    input.linesIterator.map({ line =>
      val Seq(parent, child) = line.split(')').toSeq
      child -> parent
    }).toMap
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countOrbits(parseParentMap(input)))
  }
}
