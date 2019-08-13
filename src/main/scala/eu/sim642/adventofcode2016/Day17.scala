package eu.sim642.adventofcode2016

import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.graph._

object Day17 {

  case class Box(min: Pos, max: Pos) {
    def contains(pos: Pos): Boolean = {
      min.x <= pos.x && pos.x <= max.x &&
        min.y <= pos.y && pos.y <= max.y
    }
  }

  private val startPos = Pos(0, 0)
  private val targetPos = Pos(3, 3)
  private val vaultBox = Box(startPos, targetPos)

  private val moveOffsets = Seq( // in order
    'U' -> Pos(0, -1),
    'D' -> Pos(0, 1),
    'L' -> Pos(-1, 0),
    'R' -> Pos(1, 0),
  )

  case class VaultPos(passcode: String, pos: Pos, path: String) {
    private lazy val doors = {
      Day14.md5(passcode + path).take(4).map(_ >= 'b')
    }

    def moves: Seq[VaultPos] = {
      if (pos == targetPos) // "paths always end the first time they reach the bottom-right room"
        Seq.empty
      else {
        for {
          ((move, offset), isOpen) <- moveOffsets.zip(doors)
          if isOpen
          newPos = pos + offset
          if vaultBox.contains(newPos)
        } yield copy(pos = newPos, path = path + move)
      }
    }
  }

  def shortestVaultPath(passcode: String): String = {

    val graphSearch = new GraphSearch[VaultPos] with UnitNeighbors[VaultPos] with Heuristic[VaultPos] {
      override val startNode: VaultPos = VaultPos(passcode, startPos, "")

      override def unitNeighbors(vaultPos: VaultPos): TraversableOnce[VaultPos] = vaultPos.moves

      override def isTargetNode(vaultPos: VaultPos, dist: Int): Boolean = vaultPos.pos == targetPos

      override def heuristic(vaultPos: VaultPos): Int = vaultPos.pos manhattanDistance targetPos
    }

    AStar.search(graphSearch).target.get._1.path
  }

  def longestVaultPathLength(passcode: String): Int = {

    val graphTraversal = new GraphTraversal[VaultPos] with UnitNeighbors[VaultPos] {
      override val startNode: VaultPos = VaultPos(passcode, startPos, "")

      override def unitNeighbors(vaultPos: VaultPos): TraversableOnce[VaultPos] = vaultPos.moves
    }

    BFS.traverse(graphTraversal).distances
      .filter(_._1.pos == targetPos)
      .values.max
  }

  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim
  val input = "yjjvjgan"

  def main(args: Array[String]): Unit = {
    println(shortestVaultPath(input))
    println(longestVaultPathLength(input))
  }
}
