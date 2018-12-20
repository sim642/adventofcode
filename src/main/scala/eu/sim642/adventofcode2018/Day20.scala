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


  private val originPos = Pos(0, 0) // arbitrary origin position

  private val moveOffsets = Map(
    'N' -> Pos(0, -1),
    'E' -> Pos(1, 0),
    'S' -> Pos(0, 1),
    'W' -> Pos(-1, 0),
  )

  def movePositions(moves: List[Char], pos: Pos = originPos): List[Pos] = moves match {
    case Nil => List(pos)
    case move :: tl =>
      val newPos = pos + moveOffsets(move)
      pos :: movePositions(tl, newPos)
  }

  type Doors = mutable.Map[Pos, Set[Pos]]

  def allDoors(doors: Doors, regexNode: RegexNode, poss: Set[Pos] = Set(originPos)): Set[Pos] = regexNode match {
    case StringNode(s) =>
      val posMoves = poss.map(movePositions(s.toList, _))

      for {
        moves <- posMoves
        (p1, p2) <- moves.zip(moves.tail)
      } {
        doors(p1) += p2
        doors(p2) += p1
      }

      posMoves.map(_.last)

      // not sure if this is any faster
      /*s.foldLeft(poss)({ (acc, c) =>
        val offset = moveOffsets(c)
        for {
          pos <- acc
          newPos = pos + offset
        } yield {
          doors(pos) += newPos
          doors(newPos) += pos
          newPos
        }
      })*/

    case ChoiceNode(choices) =>
      choices.flatMap(allDoors(doors, _, poss)).toSet

    case ConcatNode(concats) =>
      concats.foldLeft(poss)({ (acc, node) =>
        allDoors(doors, node, acc)
      })
  }

  def bfs(doors: Map[Pos, Set[Pos]], startPos: Pos = originPos): Map[Pos, Int] = {

    def helper(visited: Map[Pos, Int], toVisit: Map[Pos, Int]): Map[Pos, Int] = {
      val neighbors = for {
        (pos, dist) <- toVisit
        newPos <- doors(pos)
      } yield newPos -> (dist + 1)
      val newVisited = visited ++ toVisit
      //val newToVisit = neighbors -- visited.keys
      val newToVisit = neighbors.filterKeys(!visited.contains(_)) // more efficient than -- because visited is large
      if (newToVisit.isEmpty)
        newVisited
      else
        helper(newVisited, newToVisit)
    }

    helper(Map.empty, Map(startPos -> 0))
  }

  def roomDistances(input: String): Map[Pos, Int] = {
    val regex = parseInput(input)

    val doors: Doors = mutable.Map.empty.withDefaultValue(Set.empty)
    /*for {
      moveString <- allStrings(regex)
      moves = movePositions(moveString.toList)
      (p1, p2) <- moves.zip(moves.tail)
    } {
      doors(p1) += p2
      doors(p2) += p1
    }*/
    allDoors(doors, regex)

    bfs(doors.toMap)
  }

  def furthestRoom(input: String): Int = {
    roomDistances(input).values.max
  }

  def farRooms(input: String, threshold: Int = 1000): Int = {
    roomDistances(input).values.count(_ >= threshold)
  }

  def parseInput(input: String): RegexNode = {

    def inputRegexNode: Parser[RegexNode] = "^" ~> concatNode <~ "$"

    def concatNode: Parser[ConcatNode] = rep1(regexNode) ^^ ConcatNode

    def emptyNode: Parser[StringNode] = "" ^^^ StringNode("")

    def regexNode: Parser[RegexNode] = (
      "[NESW]+".r ^^ StringNode
    | "(" ~> repsep(concatNode | emptyNode, "|") <~ ")" ^^ ChoiceNode
    )

    parseAll(inputRegexNode, input) match {
      case Success(result, next) => result
      case NoSuccess(msg, next) => throw new RuntimeException(s"Regex parsing error: $msg ($next)")
    }
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(furthestRoom(input))
    println(farRooms(input))
  }
}
