package eu.sim642.adventofcode2019

import eu.sim642.adventofcode2019.Day9._
import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcode2018.Day13.DirectionPos

object Day17 {

  def sumAlignmentParameters(grid: Grid[Char]): Int = {
    (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell == '#'
      pos = Pos(x, y)
      neighbors = Pos.axisOffsets.map(pos + _).filter(grid.containsPos)
      if neighbors.forall(grid(_) == '#')
    } yield pos.x * pos.y).sum
  }

  sealed trait Move
  case object Left extends Move {
    override def toString: String = "L"
  }
  case object Right extends Move {
    override def toString: String = "R"
  }
  case class Forward(n: Int) extends Move {
    override def toString: String = n.toString
  }

  type Path = List[Move]

  def pathToString(path: Path): String = path.mkString("", ",", "")

  def findRobot(grid: Grid[Char]): (Pos, Pos) = {
    def parseRobotDirection(cell: Char): Option[Pos] = cell match {
      case '>' => Some(Pos(1, 0))
      case '<' => Some(Pos(-1, 0))
      case 'v' => Some(Pos(0, 1))
      case '^' => Some(Pos(0, -1))
      case _ => None
    }

    (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      pos = Pos(x, y)
      direction <- parseRobotDirection(cell)
    } yield (pos, direction)).head
  }

  def getPath(grid: Grid[Char]): Path = {

    def getCell(pos: Pos): Char = {
      if (grid.containsPos(pos))
        grid(pos)
      else
        '.'
    }

    def helper(pos: Pos, direction: Pos): Path = {
      if (getCell(pos + direction) == '#') { // move forward
        helper(pos + direction, direction) match {
          case Forward(n) :: tl => Forward(n + 1) :: tl
          case tl => Forward(1) :: tl
        }
      }
      else if (getCell(pos + direction.left) == '#') { // turn left
        Left :: helper(pos, direction.left)
      }
      else if (getCell(pos + direction.right) == '#') { // turn right
        Right :: helper(pos, direction.right)
      }
      else { // end of line
        Nil
      }
    }

    val (pos, direction) = findRobot(grid)
    helper(pos, direction)
  }

  private val factorPathMaxLength: Int = 20 // comma delimited
  private val factorPathMaxLength2: Int = (20 + 1) / 2 // comma delimited

  def factorPath(path: Path): Seq[Path] = {
    for {
      n <- (1 to factorPathMaxLength2).reverse
      (init, tail) = path.splitAt(n)
      if pathToString(init).lengthIs <= factorPathMaxLength
      if tail.containsSlice(init)
    } {
      println(init)
    }

    // TODO: complete path factoring
    ???
  }

  def dustCollected(program: Memory, grid: Grid[Char]): Int = {
    val path = getPath(grid)
    println(pathToString(path))
    //println(factorPath(path))

    // manually factored for my input...
    /*

    R,6,L,10,R,10,R,10,L,10,L,12,R,10,R,6,L,10,R,10,R,10,L,10,L,12,R,10,R,6,L,10,R,10,R,10,R,6,L,12,L,10,R,6,L,10,R,10,R,10,R,6,L,12,L,10,L,10,L,12,R,10,R,6,L,12,L,10
    A = R,6,L,10,R,10,R,10
    A,L,10,L,12,R,10,A,L,10,L,12,R,10,A,R,6,L,12,L,10,A,R,6,L,12,L,10,L,10,L,12,R,10,R,6,L,12,L,10
    B = L,10,L,12,R,10
    A,B,A,B,A,R,6,L,12,L,10,A,R,6,L,12,L,10,B,R,6,L,12,L,10
    C = R,6,L,12,L,10
    A,B,A,B,A,C,A,C,B,C

     */

    val newProgram = program + (0 -> 2L)

    val inputString =
      """A,B,A,B,A,C,A,C,B,C
        |R,6,L,10,R,10,R,10
        |L,10,L,12,R,10
        |R,6,L,12,L,10
        |n
        |""".stripMargin
    val inputs = inputString.map(_.toLong).to(LazyList)

    ProgramState(newProgram, inputs = inputs).outputs.last.toInt
  }

  def printGrid(grid: Grid[Char]): Unit = {
    for (row <- grid) {
      for (cell <- row)
        print(cell)
      println()
    }
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  def parseInput(input: String): Grid[Char] = {
    parseGrid(ProgramState(parseProgram(input)).outputs.map(_.toChar).mkString)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    printGrid(parseInput(input))
    println(sumAlignmentParameters(parseInput(input)))
    println(dustCollected(parseProgram(input), parseInput(input))) // 962913

    // 46 - not right
  }
}
