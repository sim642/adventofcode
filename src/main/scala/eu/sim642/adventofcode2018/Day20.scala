package eu.sim642.adventofcode2018

import Day20.RegexNode._
import scala.util.parsing.combinator._
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.graph.{BFS, GraphSearch, GraphTraversal, UnitNeighbors}

import scala.collection.mutable

object Day20 extends RegexParsers {

  enum RegexNode {
    case StringNode(s: String)
    case ChoiceNode(choices: Seq[RegexNode])
    case ConcatNode(concats: Seq[RegexNode])
  }

  def allStrings(regexNode: RegexNode): Iterator[String] = regexNode match {
    case StringNode(s) => Iterator(s)
    case ChoiceNode(choices) => choices.flatMap(allStrings).iterator
    case ConcatNode(concats) => concats.foldLeft(Iterator(""))({ (acc, node) =>
      // works on actual inputs, but doesn't construct all strings...
      // nodeStrings iterator gets exhausted after first, other branches ignored
      /*val nodeStrings = allStrings(node)
      for {
        s1 <- acc
        s2 <- nodeStrings
      } yield s1 + s2*/

      // correctly constructs all strings, too much for actual inputs
      for {
        s1 <- acc
        s2 <- allStrings(node)
      } yield s1 + s2
    })
  }


  private val originPos = Pos.zero // arbitrary origin position

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

  def bfs(doors: collection.Map[Pos, Set[Pos]], startPos: Pos = originPos): collection.Map[Pos, Int] = {

    val graphTraversal = new GraphTraversal[Pos] with UnitNeighbors[Pos] {
      override val startNode: Pos = startPos

      override def unitNeighbors(pos: Pos): IterableOnce[Pos] = doors(pos)
    }

    BFS.traverse(graphTraversal).distances
  }

  def roomDistances(input: String): collection.Map[Pos, Int] = {
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

    bfs(doors)
  }

  def furthestRoom(input: String): Int = {
    roomDistances(input).values.max
  }

  def farRooms(input: String, threshold: Int = 1000): Int = {
    roomDistances(input).values.count(_ >= threshold)
  }

  def parseInput(input: String): RegexNode = {

    def inputRegexNode: Parser[RegexNode] = "^" ~> concatNode <~ "$"

    def concatNode: Parser[ConcatNode] = rep1(regexNode) ^^ ConcatNode.apply

    def emptyNode: Parser[StringNode] = "" ^^^ StringNode("")

    //noinspection ScalaUnnecessaryParentheses
    def regexNode: Parser[RegexNode] = (
      "[NESW]+".r ^^ StringNode.apply
    | "(" ~> repsep(concatNode | emptyNode, "|") <~ ")" ^^ ChoiceNode.apply
    )

    parseAll(inputRegexNode, input) match {
      case Success(result, next) => result
      case noSuccess: NoSuccess => throw new RuntimeException(s"Regex parsing error: ${noSuccess.msg} (${noSuccess.next})")
    }
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(furthestRoom(input))
    println(farRooms(input))
  }
}
