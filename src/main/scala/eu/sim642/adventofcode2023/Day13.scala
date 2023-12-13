package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid

object Day13 {

  def findMirrorHorizontal(grid: Grid[Boolean]): Option[Int] = {
    (1 to (grid.size - 1))
      .find(mirrorY => {
        (1 to (mirrorY min (grid.size - mirrorY)))
          .forall(d =>
            grid(mirrorY - d) == grid(mirrorY + d - 1)
          )
      })
  }

  var i = -1

  def summarizeMirror(grid: Grid[Boolean]): Int = {
    i += 1
    findMirrorHorizontal(grid) match
      case Some(y) => 100 * y
      case None =>
        findMirrorHorizontal(grid.transpose) match
          case Some(x) => x
          case None => throw new IllegalArgumentException(i.toString)
  }

  def summarizeMirrors(grids: Seq[Grid[Boolean]]): Int = grids.map(summarizeMirror).sum

  def parseGrid(s: String): Grid[Boolean] = s.linesIterator.map(_.map(_ == '#').toVector).toVector

  def parseGrids(input: String): Seq[Grid[Boolean]] = input.split("\n\n").map(parseGrid).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day13.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(summarizeMirrors(parseGrids(input)))
  }
}
