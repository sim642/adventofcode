package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcode2018.Day13.DirectionPos
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.IntegralImplicits._

import java.util.StringTokenizer
import scala.jdk.CollectionConverters.*


object Day22 {

  enum Move {
    case Forward(n: Int)
    case Left
    case Right
  }

  case class Input(map: Grid[Char], path: Seq[Move])

  case class PosFacing(pos: Pos, facing: Pos)

  def followPath(input: Input, wrap: PosFacing => PosFacing): PosFacing = {
    val Input(map, path) = input

    def moveForward(posFacing: PosFacing): PosFacing = {
      val PosFacing(pos, facing) = posFacing

      val newPos = pos + facing
      val newPosFacing =
        if (!map.containsPos(newPos) || map(newPos) == ' ')
          wrap(posFacing)
        else
          PosFacing(newPos, facing)

      map(newPosFacing.pos) match {
        case '.' => newPosFacing
        case '#' => posFacing
      }
    }

    def move(posFacing: PosFacing, move: Move): PosFacing = {
      move match {
        case Move.Forward(n) => (0 until n).foldLeft(posFacing)((acc, _) => moveForward(acc))
        case Move.Left => posFacing.copy(facing = posFacing.facing.left)
        case Move.Right => posFacing.copy(facing = posFacing.facing.right)
      }
    }

    val initialX = map.head.indexOf('.')
    val initialPosFacing = PosFacing(Pos(initialX, 0), Pos(1, 0))

    path.foldLeft(initialPosFacing)(move)
  }

  private val facingPassword = Map(
    Pos(1, 0) -> 0,
    Pos(0, 1) -> 1,
    Pos(-1, 0) -> 2,
    Pos(0, -1) -> 3,
  )

  def posFacingPassword(posFacing: PosFacing): Int = {
    val PosFacing(Pos(x, y), facing) = posFacing
    1000 * (y + 1) + 4 * (x + 1) + facingPassword(facing)
  }

  trait Part {
    def finalPassword(input: Input): Int
  }

  object Part1 extends Part {

    override def finalPassword(input: Input): Int = {
      val Input(map, _) = input
      val mapTranspose = map.transpose

      // TODO: simplify?
      def wrap(posFacing: PosFacing): PosFacing = {
        val PosFacing(pos, facing) = posFacing
        val newPos = facing match {
          case Pos(1, 0) => Pos(map(pos.y).indexWhere(_ != ' '), pos.y)
          case Pos(0, 1) => Pos(pos.x, mapTranspose(pos.x).indexWhere(_ != ' '))
          case Pos(-1, 0) => Pos(map(pos.y).lastIndexWhere(_ != ' '), pos.y)
          case Pos(0, -1) => Pos(pos.x, mapTranspose(pos.x).lastIndexWhere(_ != ' '))
        }
        PosFacing(newPos, facing)
      }

      val finalPosFacing = followPath(input, wrap)
      posFacingPassword(finalPosFacing)
    }
  }

  trait CubeNet {
    val edgeLength: Int
    val layout: String
    val glue: Seq[(Int, Pos, Int, Pos)]
  }

  def cubeNetWrap(cubeNet: CubeNet): PosFacing => PosFacing = {
    import cubeNet._

    val layoutGrid = layout.linesIterator.map(_.toVector).toVector
    val facePoss = (0 until 6).map(i => layoutGrid.posOf('0' + i))

    val glueMap = glue.flatMap({ case (face1, edge1, face2, edge2) =>
      val facePos1 = facePoss(face1)
      val facePos2 = facePoss(face2)
      Seq(
        (facePos1, edge1) -> (facePos2, edge2),
        (facePos2, edge2) -> (facePos1, edge1),
      )
    }).toMap

    def edgeCorner(edge: Pos): Pos = edge match {
      case Pos(1, 0) => Pos(edgeLength - 1, 0)
      case Pos(0, 1) => Pos(edgeLength - 1, edgeLength - 1)
      case Pos(-1, 0) => Pos(0, edgeLength - 1)
      case Pos(0, -1) => Pos(0, 0)
    }

    def wrap(posFacing: PosFacing): PosFacing = {
      val PosFacing(pos, facing) = posFacing
      val facePos = Pos(pos.x / edgeLength, pos.y / edgeLength)
      val (newFacePos, newEdge) = glueMap((facePos, facing))
      val newFacing = -newEdge
      val inFacePos = Pos(pos.x % edgeLength, pos.y % edgeLength)
      val cornerDist = edgeCorner(facing) manhattanDistance inFacePos
      val newInFacePos = edgeCorner(newFacing) + cornerDist *: newFacing.right
      val newPos = edgeLength *: (newFacePos + newEdge) + newInFacePos + newFacing
      PosFacing(newPos, newFacing)
    }

    wrap
  }

  /**
   * Cube net for example.
   */
  object ExampleCubeNet extends CubeNet {

    override val edgeLength: Int = 4

    override val layout: String =
      """  0
        |123
        |  45""".stripMargin

    override val glue: Seq[(Int, Pos, Int, Pos)] = Seq(
      // only 3 glue required for example
      (0, Pos(-1, 0), 2, Pos(0, -1)),
      (1, Pos(0, 1), 4, Pos(0, 1)),
      (3, Pos(1, 0), 5, Pos(0, -1)),
      // remaining 4 glue, not required for example
      (0, Pos(0, -1), 1, Pos(0, -1)),
      (0, Pos(1, 0), 5, Pos(1, 0)),
      (1, Pos(-1, 0), 5, Pos(0, 1)),
      (2, Pos(0, 1), 4, Pos(-1, 0)),
    )
  }


  /**
   * Cube net for input.
   * Same for all inputs: https://www.reddit.com/r/adventofcode/comments/zsgbe7/2022_day_22_question_about_your_input/j17smus/.
   */
  object InputCubeNet extends CubeNet {

    override val edgeLength: Int = 50

    override val layout: String =
      """ 01
        | 2
        |34
        |5  """.stripMargin

    override val glue: Seq[(Int, Pos, Int, Pos)] = Seq(
      (0, Pos(-1, 0), 3, Pos(-1, 0)),
      (0, Pos(0, -1), 5, Pos(-1, 0)),
      (1, Pos(0, 1), 2, Pos(1, 0)),
      (1, Pos(1, 0), 4, Pos(1, 0)),
      (1, Pos(0, -1), 5, Pos(0, 1)),
      (2, Pos(-1, 0), 3, Pos(0, -1)),
      (4, Pos(0, 1), 5, Pos(1, 0)),
    )
  }

  object Part2 extends Part {

    def finalPassword(input: Input, cubeNet: CubeNet): Int = {
      val finalPosFacing = followPath(input, cubeNetWrap(cubeNet))
      posFacingPassword(finalPosFacing)
    }

    override def finalPassword(input: Input): Int = finalPassword(input, InputCubeNet)
  }

  def parsePath(s: String): Seq[Move] = {
    val tok = new StringTokenizer(s, "LR", true)
    tok.asIterator().asScala.map({
      case "L" => Move.Left
      case "R" => Move.Right
      case s: String => Move.Forward(s.toInt)
    }).toSeq
  }

  def parseInput(input: String): Input = {
    val Array(mapStr, pathStr) = input.split("\n\n", 2)
    val map = mapStr.linesIterator.map(_.toVector).toVector.padGrid(' ')
    // pad lines just for transpose in wrap around
    val path = parsePath(pathStr)
    Input(map, path)
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day22.txt")).mkString.stripLineEnd

  def main(args: Array[String]): Unit = {
    println(Part1.finalPassword(parseInput(input)))
    println(Part2.finalPassword(parseInput(input)))

    // part 2: 120330 - too low
  }
}
