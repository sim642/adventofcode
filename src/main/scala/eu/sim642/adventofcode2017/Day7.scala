package eu.sim642.adventofcode2017

object Day7 {

  val lineRe = "(\\w+) \\((\\d+)\\)(?: -> (\\w+(?:, \\w+)*))?".r

  case class Program(name: String, weight: Int, children: Set[String])

  def bottomProgram(programs: Map[String, Program]): String = {
    val parents: Map[String, String] =
      for {
        (prog, prog2) <- programs
        child <- prog2.children
      } yield child -> prog

    programs.keys.find(!parents.contains(_)).get
  }

  def bottomProgram(input: String): String = {
    bottomProgram(parseInput(input))
  }

  private def parseInput(input: String) = {
    val children = input.lines.map(_.trim).map({
      case lineRe(program, weight, childPrograms) =>
        val childs = if (childPrograms != null) childPrograms.split(", ").toSet else Set[String]()
        program -> Program(program, weight.toInt, childs)
    }).toMap
    children
  }

  def wrongHeight(programs: Map[String, Program]): Int = {
    val bottom = bottomProgram(programs)

    def helper(program: String): Either[Int, Int] = {
      val childSeq = programs(program).children.toSeq
      val cs = childSeq.map(helper)
      cs.find(_.isLeft) match {
        case Some(either) => either
        case None =>
          val recWeights = cs.map(_.right.get)
          if (recWeights.distinct.size <= 1) {
            // all balanced
            Right(programs(program).weight + recWeights.sum)
          }
          else {
            // imbalanced
            val childWeight = childSeq.zip(recWeights).toMap
            val weightChild = (for {
              (prog, w) <- childWeight.toSeq
            } yield w -> prog).groupBy(_._1).mapValues(_.map(_._2))

            val badChild = weightChild.find(_._2.size == 1).get._2.head
            val badWeight = childWeight(badChild)
            val badw = programs(badChild).weight
            val goodWeight = weightChild.find(_._2.size > 1).get._1

            Left(goodWeight - (badWeight - badw))
          }
      }
    }

    helper(bottom).left.get
  }

  def wrongHeight(input: String): Int = {
    wrongHeight(parseInput(input))
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(bottomProgram(input))
    println(wrongHeight(input))
  }
}
