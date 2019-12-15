package eu.sim642.adventofcode2019

import eu.sim642.adventofcode2019.Day9._
import eu.sim642.adventofcodelib.graph.{BFS, GraphSearch, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos

object Day15 {

  type Status = Int

  private val inputOffsets = Seq(
    1 -> Pos(0, 1), // north
    2 -> Pos(0, -1), // south
    3 -> Pos(-1, 0), // west
    4 -> Pos(1, 0), // east
  )

  def oxygenSystemMoves(program: Memory): Int = {

    /*def dfs(programState: ProgramState, pos: Pos, map: Map[Pos, Status]): Map[Pos, Status] = {
      ???
    }

    dfs(ProgramState(program), Pos.zero, Map.empty)*/

    val graphSearch = new GraphSearch[(Pos, Status, ProgramState)] with UnitNeighbors[(Pos, Status, ProgramState)] {
      override val startNode: (Pos, Status, ProgramState) = {
        (Pos.zero, 1, ProgramState(program))
      }

      override def unitNeighbors(node: (Pos, Status, ProgramState)): IterableOnce[(Pos, Status, ProgramState)] = {
        val (pos, status, programState) = node
        for {
          (input, offset) <- inputOffsets
          (newProgramState, output) = programState.copy(inputs = LazyList(input)).outputStates.head
          newStatus = output.toInt
          if newStatus != 0
          newPos = pos + offset
        } yield (newPos, newStatus, newProgramState)
      }

      override def isTargetNode(node: (Pos, Status, ProgramState), dist: Int): Boolean = {
        val (pos, status, programState) = node
        status == 2
      }
    }

    BFS.search(graphSearch).target.get._2
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day15.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(oxygenSystemMoves(parseProgram(input)))
  }
}
