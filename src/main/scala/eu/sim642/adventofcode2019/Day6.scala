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

  def countOrbitalTransfers(parentMap: Map[String, String]): Int = {
    def getParents(obj: String): Seq[String] = {
      // TODO: unfold0 for Seq etc as well
      Seq.unfold(obj)(child => parentMap.get(child).map(parent => (parent, parent)))
    }

    val youParents = getParents("YOU")
    val sanParents = getParents("SAN")
    val commonParents = youParents.intersect(sanParents)

    val youDist = youParents.size - commonParents.size
    val sanDist = sanParents.size - commonParents.size
    youDist + sanDist
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
    println(countOrbitalTransfers(parseParentMap(input)))
  }
}
