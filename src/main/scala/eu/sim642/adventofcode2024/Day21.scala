package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.graph.*
import eu.sim642.adventofcodelib.pos.Pos

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

  trait Solution {
    def shortestSequenceLength(code: Code, directionalKeypads: Int): Long

    def codeComplexity(code: Code, directionalKeypads: Int): Long = {
      val numericPart = code.dropRight(1).toInt
      shortestSequenceLength(code, directionalKeypads) * numericPart
    }

    def sumCodeComplexity(codes: Seq[Code], directionalKeypads: Int): Long = codes.map(codeComplexity(_, directionalKeypads)).sum
  }

  object NaiveSolution extends Solution {

    case class State(directionalPoss: List[Pos], numericPos: Pos, input: Code) {

      private def numericPress(button: Char): Option[State] = button match {
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

      private def directionalPress(button: Char): Option[State] = directionalPoss match {
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

    override def shortestSequenceLength(code: Code, directionalKeypads: Int): Long = {

      val graphSearch = new GraphSearch[State] with UnitNeighbors[State] {
        override val startNode: State =
          State(List.fill(directionalKeypads)(directionalKeypad.posOf('A')), numericKeypad.posOf('A'), "")

        override def unitNeighbors(state: State): IterableOnce[State] = {
          // TODO: why not flatMap?
          "<v>^A".iterator.flatten(using state.userPress).filter(newState => code.startsWith(newState.input))
        }

        override def isTargetNode(state: State, dist: Int): Boolean = state.input == code
      }

      BFS.search(graphSearch).target.get._2
    }
  }

  object DynamicProgrammingSolution extends Solution {

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

    private val offsetDirectionals: Map[Pos, Char] = directionalOffsets.map(_.swap)

    private def keypadPaths(keypad: Grid[Char]): Map[(Char, Char), Set[Code]] = {
      val box = Box(Pos.zero, Pos(keypad(0).size - 1, keypad.size - 1))
      // TODO: use one traversal per position (to all other positions), instead of pairwise
      (for {
        startPos <- box.iterator
        if keypad(startPos) != ' '
        targetPos <- box.iterator
        if keypad(targetPos) != ' '
      } yield {
        // TODO: use multi-predecessor BFS to construct all shortest paths
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
                .map({ (p2, p1) => offsetDirectionals(p1 - p2) })
                .mkString
            )
            .toSet
      }).toMap
    }

    private val numericPaths: Map[(Char, Char), Set[Code]] = keypadPaths(numericKeypad)
    private val directionalPaths: Map[(Char, Char), Set[Code]] = keypadPaths(directionalKeypad)

    def keypadPaths(keypad: Int): Map[(Char, Char), Set[Code]] = if (keypad == 0) numericPaths else directionalPaths

    override def shortestSequenceLength(code: Code, directionalKeypads: Int): Long = {
      val memo = mutable.Map.empty[(Code, Int), Long]

      def helper(code: Code, keypad: Int): Long = {
        if (keypad == directionalKeypads + 1)
          code.length
        else {
          memo.getOrElseUpdate((code, keypad), {
            (("A" + code) lazyZip code) // start moving from A
              .map({ (prev, cur) =>
                keypadPaths(keypad)((prev, cur))
                  .map(path => helper(path + 'A', keypad + 1)) // end at A to press
                  .min
              })
              .sum
          })
        }
      }

      helper(code, 0)
    }
  }

  def parseCodes(input: String): Seq[Code] = input.linesIterator.toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  val part1DirectionalKeypads = 2
  val part2DirectionalKeypads = 25

  def main(args: Array[String]): Unit = {
    import DynamicProgrammingSolution.*
    println(sumCodeComplexity(parseCodes(input), part1DirectionalKeypads))
    println(sumCodeComplexity(parseCodes(input), part2DirectionalKeypads))

    // part 2: 1301407762 - too low (Int overflowed in shortestSequenceLength2)
  }
}
