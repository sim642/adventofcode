package eu.sim642.adventofcode2017

import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder

object Day16 {

  type Name = Char

  sealed trait DanceMove
  case class Spin(n: Int) extends DanceMove
  case class Exchange(a: Int, b: Int) extends DanceMove
  case class Partner(a: Name, b: Name) extends DanceMove

  private val spinRegex = """s(\d+)""".r
  private val exchangeRegex = """x(\d+)/(\d+)""".r
  private val partnerRegex = """p(\w)/(\w)""".r

  def parseMove(moveStr: String): DanceMove = moveStr match {
    case spinRegex(n) => Spin(n.toInt)
    case exchangeRegex(a, b) => Exchange(a.toInt, b.toInt)
    case partnerRegex(a, b) => Partner(a(0), b(0))
  }

  def parseMoves(movesStr: String): Seq[DanceMove] = movesStr.split(",").toSeq.map(parseMove)

  def applyMove(programs: Vector[Name], move: DanceMove): Vector[Name] = move match {
    case Spin(n) => programs.takeRight(n) ++ programs.dropRight(n)
    case Exchange(a, b) =>
      programs.updated(a, programs(b)).updated(b, programs(a))
    case Partner(a, b) =>
      val i = programs.indexOf(a)
      val j = programs.indexOf(b)
      programs.updated(i, b).updated(j, a)
  }

  def applyMoves(moves: Seq[DanceMove], programs: Vector[Name]): Vector[Name] = moves.foldLeft(programs)(applyMove)

  def applyMoves(movesStr: String, programsStr: String = ('a' to 'p').mkString): String = applyMoves(parseMoves(movesStr), programsStr.toVector).mkString

  /*def applyMovesRepeat(moves: Seq[DanceMove], programs: Vector[Name], repeat: Int): Vector[Name] =
    (0 until repeat).foldLeft(programs)((programs, _) => applyMoves(moves, programs))*/
  def applyMovesRepeat(moves: Seq[DanceMove], programs: Vector[Name], repeat: Int): Vector[Name] = {
    NaiveCycleFinder.find(programs, applyMoves(moves, _: Vector[Name]))(repeat)
  }

  def applyMovesRepeat(movesStr: String, programsStr: String = ('a' to 'p').mkString, repeat: Int = 1000000000): String =
    applyMovesRepeat(parseMoves(movesStr), programsStr.toVector, repeat).mkString

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(applyMoves(input))
    println(applyMovesRepeat(input))
  }
}
