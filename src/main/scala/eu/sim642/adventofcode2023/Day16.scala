package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.graph.{BFS, DFS, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcode2018.Day13.DirectionPos
import eu.sim642.adventofcodelib.GridImplicits.*

import scala.annotation.tailrec
import scala.collection.mutable

object Day16 {

  case class Beam(pos: Pos, direction: Pos)

  def countEnergized(grid: Grid[Char], startBeam: Beam = Beam(Pos.zero, Pos(1, 0))): Int = {

    val graphTraversal = new GraphTraversal[Beam] with UnitNeighbors[Beam] {
      override val startNode: Beam = startBeam

      override def unitNeighbors(beam: Beam): IterableOnce[Beam] = {
        val Beam(pos, direction) = beam
        val newDirections =
          grid(pos) match {
            case '.' => Iterator(direction)
            case '/' => Iterator(direction.reflectMinor)
            case '\\' => Iterator(direction.reflectMajor)
            case '|' if direction.x == 0 => Iterator(direction)
            case '-' if direction.y == 0 => Iterator(direction)
            case '|' | '-' => Iterator(direction.left, direction.right)
          }
        for {
          newDirection <- newDirections
          newPos = pos + newDirection
          if grid.containsPos(newPos)
        } yield Beam(newPos, newDirection)
      }
    }

    def f(x: Beam)(get: Beam => Set[Pos]): Set[Pos] =
      //graphTraversal.unitNeighbors(x).iterator.flatMap(get).toSet + x.pos
      graphTraversal.unitNeighbors(x).iterator.map(get).foldLeft(Set.empty[Pos])(_ ++ _) + x.pos

    val stable = mutable.Set.empty[Beam]
    val called = mutable.Set.empty[Beam]
    val infl = mutable.Map.empty[Beam, Set[Beam]].withDefaultValue(Set.empty[Beam])
    val rho = mutable.Map.empty[Beam, Set[Pos]].withDefaultValue(Set.empty[Pos])

    def destabilize(x: Beam): Unit = {
      val w = infl(x)
      infl(x) = Set.empty
      for (y <- w) {
        stable -= y
        if (!called(y))
          destabilize(y)
      }
    }

    def eval(x: Beam)(y: Beam): Set[Pos] = {
      solve(y)
      infl(y) += x
      rho(y)
    }


    @tailrec
    def solve(x: Beam): Unit = {
      println((rho.size, stable.size, called.size))
      if (!stable(x) && !called(x)) {
        stable += x
        called += x
        //val tmp = rho(x) union f(x)(eval(x))
        val tmp = f(x)(eval(x))
        called -= x
        if (rho(x) != tmp) {
          println(s"  ${tmp.size}")
          rho(x) = tmp
          destabilize(x)
          solve(x)
        }
      }
    }


    //println(DFS.traverse(graphTraversal).nodes.size)
    //DFS.traverse(graphTraversal).nodes.map(_.pos).size

    solve(graphTraversal.startNode)
    //print(rho)
    rho(graphTraversal.startNode).size
  }

  def maxEnergized(grid: Grid[Char]): Int = {
    val top = grid(0).indices.map(x => Beam(Pos(x, 0), Pos(0, 1)))
    val bottom = grid(0).indices.map(x => Beam(Pos(x, grid.size - 1), Pos(0, -1)))
    val left = grid.indices.map(y => Beam(Pos(0, y), Pos(1, 0)))
    val right = grid.indices.map(y => Beam(Pos(grid(0).size - 1, y), Pos(-1, 0)))
    (top ++ bottom ++ left ++ right).map(countEnergized(grid, _)).max
  }


  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countEnergized(parseGrid(input)))
    println(maxEnergized(parseGrid(input)))

    // part 2: 7222 - too low (right pos coordinates swapped in maxEnergized)
  }
}
