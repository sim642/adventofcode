package eu.sim642.adventofcode2018

import scala.util.parsing.combinator._
import eu.sim642.adventofcode2017.Day3.Pos

import scala.collection.mutable

object Day20 extends RegexParsers {

  sealed trait RegexNode
  case class StringNode(s: String) extends RegexNode
  case class ChoiceNode(choices: Seq[RegexNode]) extends RegexNode
  case class ConcatNode(concats: Seq[RegexNode]) extends RegexNode

  def allStrings(regexNode: RegexNode): Iterator[String] = regexNode match {
    case StringNode(s) => Iterator(s)
    case ChoiceNode(choices) => choices.flatMap(allStrings).toIterator
    case ConcatNode(concats) => concats.foldLeft(Iterator(""))({ (acc, node) =>
      val nodeStrings = allStrings(node)
      for {
        s1 <- acc
        s2 <- nodeStrings
      } yield s1 + s2
    })
  }

  private val moveOffsets = Map(
    'N' -> Pos(0, -1),
    'E' -> Pos(1, 0),
    'S' -> Pos(0, 1),
    'W' -> Pos(-1, 0),
  )

  def movePositions(moves: List[Char], pos: Pos = Pos(0, 0)): List[Pos] = moves match {
    case Nil => List(pos)
    case move :: tl =>
      val newPos = pos + moveOffsets(move)
      pos :: movePositions(tl, newPos)
  }

  def bfs(doors: Map[Pos, Set[Pos]], startPos: Pos = Pos(0, 0)): Map[Pos, Int] = {

    def helper(visited: Map[Pos, Int], toVisit: Map[Pos, Int]): Map[Pos, Int] = {
      val neighbors = for {
        (pos, dist) <- toVisit
        newPos <- doors(pos)
      } yield newPos -> (dist + 1)
      val newVisited = visited ++ toVisit
      val newToVisit = neighbors -- visited.keys
      if (newToVisit.isEmpty)
        newVisited
      else
        helper(newVisited, newToVisit)
    }

    helper(Map.empty, Map(startPos -> 0))
  }

  def furthestRoom(input: String): Int = {
    val regex = parseInput(input)

    val doors: mutable.Map[Pos, Set[Pos]] = mutable.Map.empty.withDefaultValue(Set.empty)
    for {
      moveString <- allStrings(regex)
      moves = movePositions(moveString.toList)
      (p1, p2) <- moves.zip(moves.tail)
    } {
      doors(p1) += p2
      doors(p2) += p1
    }

    //println(doors)
    val distances = bfs(doors.toMap)
    //println(distances)
    distances.values.max
  }

  def parseInput(input: String): RegexNode = {

    def inputRegexNode: Parser[RegexNode] = "^" ~> concatNode <~ "$"

    def concatNode: Parser[ConcatNode] = rep1(regexNode) ^^ ConcatNode

    def regexNode: Parser[RegexNode] = (
      "[NESW]+".r ^^ StringNode
    | "(" ~> repsep(concatNode | "" ^^^ StringNode(""), "|") <~ ")" ^^ ChoiceNode
    )

    parseAll(inputRegexNode, input) match {
      case Success(result, next) => result
      case NoSuccess(msg, next) =>
        println(msg)
        println(next)
        ???
    }
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(furthestRoom(input))
  }
}
