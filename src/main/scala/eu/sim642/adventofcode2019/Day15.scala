package eu.sim642.adventofcode2019

import Intcode._
import eu.sim642.adventofcodelib.graph.{BFS, GraphSearch, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos

object Day15 {

  // TODO: use enum?
  type Status = Int
  private val STATUS_WALL = 0
  private val STATUS_EMPTY = 1
  private val STATUS_OXYGEN_SYSTEM = 2

  private val inputOffsets = Seq(
    1 -> Pos(0, 1), // north
    2 -> Pos(0, -1), // south
    3 -> Pos(-1, 0), // west
    4 -> Pos(1, 0), // east
  )

  // only use pos for equality in BFS, not status or program state
  case class Node(pos: Pos)(val status: Status, val programState: ProgramState) {

    def neighbors: IterableOnce[Node] = {
      for {
        (input, offset) <- inputOffsets
        (newProgramState, output) = programState.copy(inputs = LazyList(input)).outputStates.head
        newStatus = output.toInt
        if newStatus != STATUS_WALL
        newPos = pos + offset
      } yield Node(newPos)(newStatus, newProgramState)
    }
  }

  def findOxygenSystem(program: Memory): (Node, Int) = {

    val graphSearch = new GraphSearch[Node] with UnitNeighbors[Node] {
      override val startNode: Node = Node(Pos.zero)(STATUS_EMPTY, ProgramState(program))

      override def unitNeighbors(node: Node): IterableOnce[Node] = node.neighbors

      override def isTargetNode(node: Node, dist: Int): Boolean = node.status == STATUS_OXYGEN_SYSTEM
    }

    BFS.search(graphSearch).target.get
  }

  def oxygenSystemMoves(program: Memory): Int = findOxygenSystem(program)._2

  def oxygenFillMinutes(program: Memory): Int = {
    val oxygenSystemNode = findOxygenSystem(program)._1

    val graphTraversal = new GraphTraversal[Node] with UnitNeighbors[Node] {
      override val startNode: Node = oxygenSystemNode

      override def unitNeighbors(node: Node): IterableOnce[Node] = node.neighbors
    }

    BFS.traverse(graphTraversal).distances.values.max
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day15.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(oxygenSystemMoves(parseProgram(input)))
    println(oxygenFillMinutes(parseProgram(input)))

    // 393 - too high
  }
}
