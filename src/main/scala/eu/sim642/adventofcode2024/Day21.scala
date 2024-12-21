package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.graph.{BFS, GraphSearch, GraphTraversal, Heuristic, SimultaneousBFS, TargetNode, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.box.Box

import scala.collection.mutable

object Day21 {

  type Code = String

  private val numericKeypad: Grid[Char] = Vector(
    Vector('7', '8', '9'),
    Vector('4', '5', '6'),
    Vector('1', '2', '3'),
    Vector(' ', '0', 'A'),
  )

  private val directionalKeypad: Grid[Char] = Vector(
    Vector(' ', '^', 'A'),
    Vector('<', 'v', '>'),
  )

  private val directionalOffsets = Map(
    '^' -> Pos(0, -1),
    '>' -> Pos(1, 0),
    'v' -> Pos(0, 1),
    '<' -> Pos(-1, 0),
  )

  case class State(directionalPoss: List[Pos], numericPos: Pos, input: Code) {

    def numericPress(button: Char): Option[State] = button match {
      case 'A' =>
        val newButton = numericKeypad(numericPos)
        Some(copy(input = input + newButton))
      case _ =>
        val offset = directionalOffsets(button)
        val newNumericPos = numericPos + offset
        if (numericKeypad.containsPos(newNumericPos) && numericKeypad(newNumericPos) != ' ')
          Some(copy(numericPos = newNumericPos))
        else
          None // out of keypad
    }

    def directionalPress(button: Char): Option[State] = directionalPoss match {
      case Nil => numericPress(button)
      case directionalPos :: newDirectionalPoss =>
        button match {
          case 'A' =>
            val newButton = directionalKeypad(directionalPos)
            copy(directionalPoss = newDirectionalPoss).directionalPress(newButton).map(newState =>
              newState.copy(directionalPoss = directionalPos :: newState.directionalPoss)
            )
          case _ =>
            val offset = directionalOffsets(button)
            val newDirectionalPos = directionalPos + offset
            if (directionalKeypad.containsPos(newDirectionalPos) && directionalKeypad(newDirectionalPos) != ' ')
              Some(copy(directionalPoss = newDirectionalPos :: newDirectionalPoss))
            else
              None // out of keypad
        }
    }

    def userPress(button: Char): Option[State] = directionalPress(button)
  }

  def shortestSequenceLength(code: Code): Int = {

    val graphSearch = new GraphSearch[State] with UnitNeighbors[State] {
      override val startNode: State = State(List.fill(2)(directionalKeypad.posOf('A')), numericKeypad.posOf('A'), "")

      override def unitNeighbors(state: State): IterableOnce[State] = "<v>^A".iterator.flatten(state.userPress).filter(s => code.startsWith(s.input))

      override def isTargetNode(state: State, dist: Int): Boolean = state.input == code
    }

    BFS.search(graphSearch).target.get._2
  }


  // copied & modified from 2024 day 10
  // TODO: extract to library?
  def pathSearch[A](graphSearch: GraphSearch[A] & UnitNeighbors[A]): GraphSearch[List[A]] & UnitNeighbors[List[A]] = {
    new GraphSearch[List[A]] with UnitNeighbors[List[A]] {
      override val startNode: List[A] = List(graphSearch.startNode)

      override def unitNeighbors(node: List[A]): IterableOnce[List[A]] =
        graphSearch.unitNeighbors(node.head).iterator.map(_ :: node)

      override def isTargetNode(node: List[A], dist: Int): Boolean = graphSearch.isTargetNode(node.head, dist)
    }
  }

  private def keypadPaths(keypad: Grid[Char]): Map[(Char, Char), Set[Code]] = {
    val box = Box(Pos.zero, Pos(keypad(0).size - 1, keypad.size - 1))
    (for {
      startPos <- box.iterator
      if keypad(startPos) != ' '
      targetPos <- box.iterator
      if keypad(targetPos) != ' '
    } yield {
      val graphSearch = new GraphSearch[Pos] with UnitNeighbors[Pos] with TargetNode[Pos] {
        override val startNode: Pos = startPos

        override def unitNeighbors(pos: Pos): IterableOnce[Pos] =
          Pos.axisOffsets.map(pos + _).filter(keypad.containsPos).filter(keypad(_) != ' ')

        override val targetNode: Pos = targetPos
      }
      (keypad(targetPos), keypad(startPos)) -> // flipped because paths are reversed
        SimultaneousBFS.search(pathSearch(graphSearch))
          .nodes
          .filter(_.head == targetPos)
          .map(poss =>
            (poss lazyZip poss.tail)
              .map({ case (p2, p1) => directionalOffsets.find(_._2 == p1 - p2).get._1 })
              .mkString
          )
          .toSet
    }).toMap
  }

  private val numericPaths: Map[(Char, Char), Set[Code]] = keypadPaths(numericKeypad)
  private val directionalPaths: Map[(Char, Char), Set[Code]] = keypadPaths(directionalKeypad)

  //println(numericPaths)

  def shortestSequenceLength2(code: Code, directionalKeypads: Int, i: Int = 0): Long = {

    val memo = mutable.Map.empty[(Code, Int), Long]

    def helper(code: Code, i: Int): Long = {
      memo.getOrElseUpdate((code, i), {
        //assert(directionalKeypads == 0)
        code.foldLeft(('A', 0L))({ case ((prev, length), cur) =>
          val newLength =
            (for {
              path <- if (i == 0) numericPaths((prev, cur)) else directionalPaths((prev, cur))
              path2 = path + 'A'
              len =
                if (i == directionalKeypads)
                  path2.length.toLong
                else
                  helper(path2, i + 1)
            } yield len).min
          (cur, length + newLength)
        })._2
      })
    }

    helper(code, 0)
  }


  def codeComplexity(code: Code, directionalKeypads: Int): Long = {
    val numericPart = code.dropRight(1).toInt
    shortestSequenceLength2(code, directionalKeypads) * numericPart
  }

  def sumCodeComplexity(codes: Seq[Code], directionalKeypads: Int): Long = codes.map(codeComplexity(_, directionalKeypads)).sum

  def parseCodes(input: String): Seq[Code] = input.linesIterator.toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumCodeComplexity(parseCodes(input), 2))
    println(sumCodeComplexity(parseCodes(input), 25))

    // part 2: 1301407762 - too low (Int overflowed in shortestSequenceLength2)
  }
}
