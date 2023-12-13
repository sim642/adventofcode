package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid

object Day13 {

  trait Part {
    val mismatches: Int

    def findMirrorHorizontal(grid: Grid[Boolean]): Option[Int] = {
      (1 until grid.size)
        .find(mirrorY => {
          (1 to (mirrorY min (grid.size - mirrorY)))
            .map(d =>
              (grid(mirrorY - d) lazyZip grid(mirrorY + d - 1))
                .count(_ != _)
            )
            .sum == mismatches
        })
    }

    def summarizeMirror(grid: Grid[Boolean]): Int = {
      findMirrorHorizontal(grid) match {
        case Some(y) => 100 * y
        case None => findMirrorHorizontal(grid.transpose).get
      }
    }

    def summarizeMirrors(grids: Seq[Grid[Boolean]]): Int = grids.map(summarizeMirror).sum
  }

  object Part1 extends Part {
    override val mismatches: Int = 0
  }

  object Part2 extends Part {
    override val mismatches: Int = 1
  }

  def parseGrid(s: String): Grid[Boolean] = s.linesIterator.map(_.map(_ == '#').toVector).toVector

  def parseGrids(input: String): Seq[Grid[Boolean]] = input.split("\n\n").map(parseGrid).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day13.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.summarizeMirrors(parseGrids(input)))
    println(Part2.summarizeMirrors(parseGrids(input)))
  }
}
