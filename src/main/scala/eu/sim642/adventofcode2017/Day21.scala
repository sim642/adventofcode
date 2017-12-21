package eu.sim642.adventofcode2017

object Day21 {

  type Grid[A] = Vector[Vector[A]]

  implicit class GridOps[A](grid: Grid[A]) {
    def rotateCW: Grid[A] = {
      val h = grid.size
      val w = grid(0).size

      /*
      1 2 3
      4 5 6

      4 1
      5 2
      6 3
       */

      Vector.tabulate(w, h)((y, x) => grid(h - 1 - x)(y))
    }

    def flipV: Grid[A] = grid.reverse

    def rotations: Set[Grid[A]] = (1 to 3).scanLeft(grid)({ case (acc, _) => acc.rotateCW }).toSet

    def symmetries: Set[Grid[A]] = grid.rotations ++ grid.flipV.rotations

    def groupedGrid(groupSize: Int): Grid[Grid[A]] =
      grid.grouped(groupSize).map(_.map(_.grouped(groupSize).toVector).transpose).toVector

    def mapGrid[B](f: A => B): Grid[B] = grid.map(_.map(f))

    def flattenGrid[B](implicit asGrid: A => Grid[B]): Grid[B] =
      grid.mapGrid(asGrid).map(_.transpose.map(_.flatten)).flatten

    def countGrid(p: A => Boolean): Int = grid.map(_.count(p)).sum
  }


  type Pattern = Grid[Boolean]

  def parsePattern(str: String, delimiter: Char = '/'): Pattern = str.split(delimiter).map(_.map(_ == '#').toVector).toVector

  private val ruleRegex = """([.#/]*) => ([.#/]*)""".r

  def parseRule(str: String): (Pattern, Pattern) = str match {
    case ruleRegex(from, to) => parsePattern(from) -> parsePattern(to)
  }

  def parseRules(input: String): Map[Pattern, Pattern] = input.lines.map(parseRule).toMap


  val initialPattern = parsePattern(
    """.#.
      |..#
      |###""".stripMargin, '\n')

  def stepPattern(rules: Map[Pattern, Pattern])(pattern: Pattern): Pattern = {
    val groupSize = (2 to 3).find(pattern.size % _ == 0).get
    pattern.groupedGrid(groupSize).mapGrid(rules).flattenGrid
  }

  def iteratePattern(rules: Map[Pattern, Pattern]): Iterator[Pattern] = {
    val completeRules = rules.flatMap({ case (from, to) => from.symmetries.map(_ -> to)})
    Iterator.iterate(initialPattern)(stepPattern(completeRules))
  }

  def countOn(rules: Map[Pattern, Pattern], iteration: Int): Int = iteratePattern(rules).drop(iteration).next().countGrid(b => b)

  def countOn(input: String, iteration: Int): Int = countOn(parseRules(input), iteration)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countOn(input, 5))
    println(countOn(input, 18))
  }
}
