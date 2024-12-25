package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._

object Day25 {

  def isKey(grid: Grid[Boolean]): Boolean =
    grid.head.forall(identity)

  def countLockKeyFits(lockKeys: Seq[Grid[Boolean]]): Int = {
    val (keys, locks) = lockKeys.partition(isKey)
    (for {
      key <- keys
      lock <- locks
      if key.correspondsGrid(lock)((k, l) => !(k && l))
    } yield ()).size
  }

  def parseLockKeys(input: String): Seq[Grid[Boolean]] = input.split("\n\n").map(_.linesIterator.map(_.map(_ == '#').toVector).toVector).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day25.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countLockKeyFits(parseLockKeys(input)))
  }
}
