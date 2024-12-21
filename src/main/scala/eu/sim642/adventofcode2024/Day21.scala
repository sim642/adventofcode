package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.graph.{BFS, GraphSearch, Heuristic, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits.*

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

  def codeComplexity(code: Code): Int = {
    val numericPart = code.dropRight(1).toInt
    shortestSequenceLength(code) * numericPart
  }

  def sumCodeComplexity(codes: Seq[Code]): Int = codes.map(codeComplexity).sum

  def parseCodes(input: String): Seq[Code] = input.linesIterator.toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumCodeComplexity(parseCodes(input)))
  }
}
