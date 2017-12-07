package eu.sim642.adventofcode2017

object Day7 {

  val lineRe = "(\\w+) \\(\\d+\\)(?: -> (\\w+(?:, \\w+)*))?".r

  def bottomProgram(children: Map[String, Set[String]]): String = {
    val (leafProgram, _) = children.find(_._2.isEmpty).get

    val parents: Map[String, String] =
      for {
        (prog, childProgs) <- children
        child <- childProgs
      } yield child -> prog

    children.keys.find(!parents.contains(_)).get
  }

  def bottomProgram(input: String): String = {
    val children = input.lines.map(_.trim).map({
      case lineRe(program, childPrograms) =>
        val childs = if (childPrograms != null) childPrograms.split(", ").toSet else Set[String]()
        program -> childs
    }).toMap

    bottomProgram(children)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(bottomProgram(input))
  }
}
