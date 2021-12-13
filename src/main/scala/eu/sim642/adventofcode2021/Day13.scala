package eu.sim642.adventofcode2021

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
  }
}
