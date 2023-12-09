package eu.sim642.adventofcode2019

import Day17.Move._
import intcode._
import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.IteratorImplicits._
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

  def sumAlignmentParameters(program: Memory): Int = sumAlignmentParameters(parseInputGrid(program))

  enum Move(override val toString: String) {
    case Left extends Move("L")
    case Right extends Move("R")
    case Forward(n: Int) extends Move(n.toString)
  }

  type Path = List[Move]

  def pathToString(path: Path): String = path.mkString(",")

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

  def split[A](seq: List[A], delimiter: List[A]): List[List[A]] = {
    val i = seq.indexOfSlice(delimiter)
    if (i < 0) {
      List(seq)
    }
    else {
      val prefix = seq.take(i)
      val suffix = seq.drop(i + delimiter.size)
      prefix +: split(suffix, delimiter)
    }
  }

  private val pathStringMaxLength: Int = 20
  private val pathMaxLength: Int = (20 + 1) / 2 // comma delimited

  def factorPathParts(pathParts: Seq[Path], maxParts: Int = 3): Iterator[List[Path]] = {
    if (pathParts.isEmpty)
      Iterator(Nil)
    else if (maxParts <= 0)
      Iterator.empty
    else {
      val firstPathPart = pathParts.head
      for {
        n <- (1 to (firstPathPart.size min pathMaxLength)).reverse.iterator
        init = firstPathPart.take(n)
        if pathToString(init).lengthIs <= pathStringMaxLength
        newPathParts = pathParts.flatMap(split(_, init).filter(_.nonEmpty))
        tailPathParts <- factorPathParts(newPathParts, maxParts - 1)
      } yield init :: tailPathParts
    }
  }

  // TODO: do this already in factorPathParts simultaneously
  def reconstructMainPaths(path: Path, pathParts: Seq[Path]): Iterator[List[Int]] = {
    if (path.isEmpty)
      Iterator(Nil)
    else {
      for {
        (pathPart, i) <- pathParts.iterator.zipWithIndex
        if path.startsWith(pathPart)
        tailPath = path.drop(pathPart.size)
        tailMainPath <- reconstructMainPaths(tailPath, pathParts)
      } yield i :: tailMainPath
    }
  }

  def mainPathToString(mainPath: Seq[Int]): String = mainPath.map(i => ('A' + i).toChar).mkString(",")

  def dustCollected(program: Memory, grid: Grid[Char]): Int = {
    val path = getPath(grid)
    val pathParts = factorPathParts(Seq(path)).head
    val mainPath = reconstructMainPaths(path, pathParts).head

    val newProgram = program + (0 -> 2L)

    val mainPathString = mainPathToString(mainPath)
    val pathPartsString = pathParts.map(pathToString).mkString("\n")
    val inputString =
      s"""$mainPathString
         |$pathPartsString
         |n
         |""".stripMargin
    val inputs = inputString.map(_.toLong).to(LazyList)

    ProgramState(newProgram, inputs = inputs).outputs.last.toInt
  }

  def dustCollected(program: Memory): Int = dustCollected(program, parseInputGrid(program))

  def printGrid(grid: Grid[Char]): Unit = {
    for (row <- grid) {
      for (cell <- row)
        print(cell)
      println()
    }
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  def parseInputGrid(program: Memory): Grid[Char] = {
    parseGrid(ProgramState(parseProgram(input)).outputs.map(_.toChar).mkString)
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    printGrid(parseInputGrid(parseProgram(input)))

    println(sumAlignmentParameters(parseProgram(input)))
    println(dustCollected(parseProgram(input))) // 962913

    // manually factored for my input for part 2...
    /*

    R,6,L,10,R,10,R,10,L,10,L,12,R,10,R,6,L,10,R,10,R,10,L,10,L,12,R,10,R,6,L,10,R,10,R,10,R,6,L,12,L,10,R,6,L,10,R,10,R,10,R,6,L,12,L,10,L,10,L,12,R,10,R,6,L,12,L,10
    A = R,6,L,10,R,10,R,10
    A,L,10,L,12,R,10,A,L,10,L,12,R,10,A,R,6,L,12,L,10,A,R,6,L,12,L,10,L,10,L,12,R,10,R,6,L,12,L,10
    B = L,10,L,12,R,10
    A,B,A,B,A,R,6,L,12,L,10,A,R,6,L,12,L,10,B,R,6,L,12,L,10
    C = R,6,L,12,L,10
    A,B,A,B,A,C,A,C,B,C

     */

    // 46 - not right
  }
}
