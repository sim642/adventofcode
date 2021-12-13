package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos

object Day13 {

  sealed trait Fold {
    def apply(dots: Set[Pos]): Set[Pos]
  }
  case class FoldUp(y: Int) extends Fold {
    override def apply(dots: Set[Pos]): Set[Pos] = dots.map({
      case Pos(dotX, dotY) if dotY > y => Pos(dotX, y - (dotY - y))
      case dot => dot
    })
  }
  case class FoldLeft(x: Int) extends Fold {
    override def apply(dots: Set[Pos]): Set[Pos] = dots.map({
      case Pos(dotX, dotY) if dotX > x => Pos(x - (dotX - x), dotY)
      case dot => dot
    })
  }

  case class Input(dots: Set[Pos], folds: Seq[Fold])


  def countDotsAfter1(input: Input): Int = input.folds.head(input.dots).size

  def printDots(dots: Set[Pos]): Unit = {
    val Box(min, max) = Box.bounding(dots)

    for (y <- min.y to max.y) {
      for (x <- min.x to max.x) {
        val pos = Pos(x, y)
        print(if (dots.contains(pos)) '#' else '.')
      }
      println()
    }
  }

  def printFoldedDots(input: Input): Unit = {
    val foldedDots = input.folds.foldLeft(input.dots)((dots, fold) => fold.apply(dots))
    printDots(foldedDots)
  }


  def parseDot(s: String): Pos = {
    val Seq(x, y) = s.split(",", 2).toSeq
    Pos(x.toInt, y.toInt)
  }

  private val foldRegex = """fold along ([xy])=(\d+)""".r

  def parseFold(s: String): Fold = s match {
    case foldRegex("y", y) => FoldUp(y.toInt)
    case foldRegex("x", x) => FoldLeft(x.toInt)
  }

  def parseInput(input: String): Input = {
    val Seq(dotsStr, foldsStr) = input.split("\n\n", 2).toSeq
    val dots = dotsStr.linesIterator.map(parseDot).toSet
    val folds = foldsStr.linesIterator.map(parseFold).toSeq
    Input(dots, folds)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day13.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countDotsAfter1(parseInput(input)))
    printFoldedDots(parseInput(input)) // RGZLBHFP
  }
}
