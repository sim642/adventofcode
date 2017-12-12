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
      val newVisited = visited union toVisit
      val newToVisit = neighbors diff visited
      if (newToVisit.isEmpty)
        newVisited
      else
        helper(newVisited, newToVisit)
    }

    helper(Set.empty, Set(startNode))
  }

  def groupSize(input: String): Int = bfs(parseNodes(input), 0).size

  def bfsGroups(nodeNeighbors: NodeNeighbors): Set[Set[Node]] = {
    if (nodeNeighbors.isEmpty)
      Set.empty
    else {
      val (startNode, _) = nodeNeighbors.head
      val group = bfs(nodeNeighbors, startNode)
      val restNodeNeighbors = nodeNeighbors.filterKeys(!group(_)).mapValues(_.filterNot(group))
      bfsGroups(restNodeNeighbors) + group
    }
  }

  def groupCount(input: String): Int = bfsGroups(parseNodes(input)).size

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(groupSize(input))
    println(groupCount(input))
  }
}
