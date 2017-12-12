package eu.sim642.adventofcode2017

object Day12 {

  type Node = Int
  type NodeNeighbors = Map[Node, Seq[Node]]

  private val nodeRegex = """(\d+) <-> (\d+(?:, \d+)*)""".r

  def parseNode(nodeInput: String): (Node, Seq[Node]) = nodeInput match {
    case nodeRegex(nodeStr, neighborsStr) =>
      nodeStr.toInt -> neighborsStr.split(", ").toSeq.map(_.toInt)
  }

  def parseNodes(nodesInput: String): NodeNeighbors = nodesInput.lines.map(parseNode).toMap

  def bfs(nodeNeighbors: NodeNeighbors, startNode: Node): Set[Node] = {

    def helper(visited: Set[Node], toVisit: Set[Node]): Set[Node] = {
      val neighbors = toVisit.flatMap(nodeNeighbors)
      val newToVisit = neighbors diff visited
      if (newToVisit.isEmpty)
        visited
      else
        helper(visited union toVisit, newToVisit)
    }

    helper(Set.empty, Set(startNode))
  }

  def groupSize(input: String): Int = bfs(parseNodes(input), 0).size

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(groupSize(input))
  }
}
