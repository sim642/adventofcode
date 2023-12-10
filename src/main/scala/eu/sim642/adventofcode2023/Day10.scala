package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.graph.{BFS, Distances, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits.*

import scala.annotation.tailrec

object Day10 {

  private val pipeDirections = Map(
    '|' -> Set(Pos(0, -1), Pos(0, 1)),
    '-' -> Set(Pos(1, 0), Pos(-1, 0)),
    'L' -> Set(Pos(0, -1), Pos(1, 0)),
    'J' -> Set(Pos(0, -1), Pos(-1, 0)),
    '7' -> Set(Pos(0, 1), Pos(-1, 0)),
    'F' -> Set(Pos(0, 1), Pos(1, 0)),
    'S' -> Pos.axisOffsets.toSet,
  )

  def findLoop(grid: Grid[Char]): Distances[Pos] = {
    val graphTraversal = new GraphTraversal[Pos] with UnitNeighbors[Pos] {
      override val startNode: Pos = grid.posOf('S')

      override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
        for {
          offset <- pipeDirections(grid(pos))
          newPos = pos + offset
          if grid.containsPos(newPos)
          if grid(newPos) != '.'
          if pipeDirections(grid(newPos)).contains(-offset)
        } yield newPos
      }
    }

    BFS.traverse(graphTraversal)
  }

  def farthestDistance(grid: Grid[Char]): Int = findLoop(grid).distances.values.max

  def enclosedTiles(grid: Grid[Char]): Int = {
    val loop = findLoop(grid).nodes

    def flood(startPos: Pos): collection.Set[Pos] = {

      val graphTraversal = new GraphTraversal[Pos] with UnitNeighbors[Pos] {
        override val startNode: Pos = startPos

        override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
          for {
            offset <- Pos.axisOffsets
            newPos = pos + offset
            if grid.containsPos(newPos)
            if !loop.contains(newPos)
          } yield newPos
        }
      }

      BFS.traverse(graphTraversal).nodes
    }

    //val outside = flood(Pos.zero)  // assuming outside
    //
    //val inside = for {
    //  outPos <- outside
    //  offset <- Pos.axisOffsets
    //  loopPos = outPos + offset
    //  if grid.containsPos(loopPos)
    //  if loop.contains(loopPos)
    //  if grid(loopPos) == '|' || grid(loopPos) == '-'
    //  inPos = loopPos + offset
    //  if grid.containsPos(inPos)
    //  if !loop.contains(inPos) && !outside.contains(inPos)
    //} yield inPos
    //
    //val inside2 = inside.flatMap(flood)

    //println(inside)

    //grid.sizeGrid - loop.size - outside.size
    //inside2.size

    val startAlternative = {
      val pos = grid.posOf('S')
      val newOffsets = for {
        offset <- pipeDirections(grid(pos))
        newPos = pos + offset
        if grid.containsPos(newPos)
        if grid(newPos) != '.'
        if pipeDirections(grid(newPos)).contains(-offset)
      } yield offset
      pipeDirections.find(_._2 == newOffsets).get._1
    }

    def isInside(pos: Pos): Boolean = {
      //println(pos)

      @tailrec
      def helper(pos: Pos, count: Int, enterEdge: Char): Boolean = {
        val newPos = pos + Pos(1, 0)
        //println(s"  $newPos")
        if (grid.containsPos(newPos)) {
          val c = if grid(newPos) == 'S' then startAlternative else if loop.contains(newPos) then grid(newPos) else '.'
          (c, enterEdge) match {
            case ('|', _) => helper(newPos, count + 1, enterEdge = '?')
            case ('.', _) => helper(newPos, count, enterEdge = '?')

            case ('-', _) => helper(newPos, count, enterEdge = enterEdge)

            case ('F', _) => helper(newPos, count, enterEdge = 'F')
            case ('7', 'F') => helper(newPos, count, enterEdge = '?')
            case ('J', 'F') => helper(newPos, count + 1, enterEdge = '?')

            case ('L', _) => helper(newPos, count, enterEdge = 'L')
            case ('7', 'L') => helper(newPos, count + 1, enterEdge = '?')
            case ('J', 'L') => helper(newPos, count, enterEdge = '?')

            //case (_, _) => helper(newPos, count, enterEdge = '?')
            case (a, b) => throw IllegalArgumentException(s"$a $b")
          }
        }
        else {
          count % 2 == 1
        }
      }

      helper(pos, 0, '?')
    }

    (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if !loop.contains(Pos(x, y))
      if isInside(Pos(x, y))
    } yield 1).sum
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(farthestDistance(parseGrid(input)))
    println(enclosedTiles(parseGrid(input)))
  }
}
