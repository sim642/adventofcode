package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2017.Day20.Pos3

object Day23 {

  case class Nanobot(pos: Pos3, radius: Int) {
    def overlaps(that: Nanobot): Boolean = (this.pos manhattanDistance that.pos) <= this.radius + that.radius
  }

  def nanobotsInLargestRadius(nanobots: Seq[Nanobot]): Int = {
    val largestRadius = nanobots.maxBy(_.radius)
    nanobots.count(nanobot => (nanobot.pos manhattanDistance largestRadius.pos) <= largestRadius.radius)
  }


  def closestMostNanobots(nanobots: Seq[Nanobot]): Int = {
    val neighbors: Map[Nanobot, Set[Nanobot]] = nanobots.map(nanobot1 => nanobot1 -> nanobots.filter(nanobot2 => nanobot2 != nanobot1 && nanobot1.overlaps(nanobot2)).toSet).toMap

    var best: Set[Nanobot] = Set.empty
    def bronKerbosh(r: Set[Nanobot], p: Set[Nanobot], x: Set[Nanobot]): Unit = {
      if (p.isEmpty && x.isEmpty) {
        //println(r)
        if (r.size > best.size)
          best = r
      }
      else {
        //val u = p.headOption.getOrElse(x.head)
        val u = (p ++ x).maxBy(neighbors(_).size) // pivot on highest degree
        var p2 = p
        var x2 = x
        for (v <- p -- neighbors(u)) {
          bronKerbosh(r + v, p2 intersect neighbors(v), x2 intersect neighbors(v))
          p2 -= v
          x2 += v
        }
      }
    }

    bronKerbosh(Set.empty, nanobots.toSet, Set.empty)
    //println(best)
    best.map(n => (n.pos manhattanDistance Pos3(0, 0, 0)) - n.radius).max
  }


  private val nanobotRegex = """pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)""".r

  def parseNanobot(s: String): Nanobot = s match {
    case nanobotRegex(x, y, z, r) => Nanobot(Pos3(x.toInt, y.toInt, z.toInt), r.toInt)
  }

  def parseInput(input: String): Seq[Nanobot] = input.lines.map(parseNanobot).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(nanobotsInLargestRadius(parseInput(input)))
    println(closestMostNanobots(parseInput(input)))

    // 90702904 - too high
  }
}
