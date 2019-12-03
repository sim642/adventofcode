package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.pos.Pos

object Day3 {

  type DirectionDistance = (Pos, Int)
  type Path = Seq[(Pos, Int)]

  def iteratePath(path: Path): Iterator[Pos] = {
    val expandedPath = path.iterator.flatMap({ case (direction, distance) =>
      Iterator.fill(distance)(direction)
    })

    expandedPath.scanLeft(Pos.zero)(_ + _)
  }

  def findCentralIntersectionDistance(paths: (Path, Path)): Int = {
    val (path1, path2) = paths
    val set1 = iteratePath(path1).toSet
    val set2 = iteratePath(path2).toSet
    val intersections = (set1 intersect set2) - Pos.zero
    val centralIntersection = intersections.minBy(_ manhattanDistance Pos.zero)
    centralIntersection manhattanDistance Pos.zero
  }

  def findClosestIntersectionDistance(paths: (Path, Path)): Int = {
    val (path1, path2) = paths
    val map1 = iteratePath(path1).zipWithIndex.toSeq.groupMapReduce(_._1)(_._2)(_ min _)
    val map2 = iteratePath(path2).zipWithIndex.toSeq.groupMapReduce(_._1)(_._2)(_ min _)
    val intersections = map1.view.filterKeys(map2.contains).toMap.transform({ case (position, distance) =>
      distance + map2(position)
    }) - Pos.zero
    intersections.minBy(_._2)._2
  }


  private val directionDistanceRegex = """([UDLR])(\d+)""".r

  private val directions = Map(
    'U' -> Pos(0, 1),
    'D' -> Pos(0, -1),
    'L' -> Pos(-1, 0),
    'R' -> Pos(1, 0),
  )

  def parseDirectionDistance(s: String): DirectionDistance = s match {
    case directionDistanceRegex(direction, distance) =>
      (directions(direction.head), distance.toInt)
  }

  def parsePath(s: String): Path = s.split(',').toSeq.map(parseDirectionDistance)

  def parseInput(input: String): (Path, Path) = {
    val Seq(path1, path2) = input.linesIterator.take(2).map(parsePath).toSeq
    (path1, path2)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day3.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(findCentralIntersectionDistance(parseInput(input)))
    println(findClosestIntersectionDistance(parseInput(input)))
  }
}
