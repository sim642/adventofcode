package eu.sim642.adventofcode2017

object Day7 {

  case class Program(name: String, weight: Int, children: Seq[String])

  private val programRegex = """(\w+) \((\d+)\)(?: -> (\w+(?:, \w+)*))?""".r("name", "weight", "children") // IDEA doesn't seem to support raw string interpolator

  def parseProgram(programStr: String): Program = programStr match {
    case programRegex(name, weight, childrenStr) =>
      val children = Option(childrenStr).map(_.split(", ").toSeq).getOrElse(Seq())
      Program(name, weight.toInt, children)
  }

  def parsePrograms(programsStr: String): Map[String, Program] = {
    programsStr.lines.map(_.trim).map(line => {
      val program = parseProgram(line)
      program.name -> program
    }).toMap
  }

  def bottomProgram(programs: Map[String, Program]): String = {
    val parents: Map[String, String] =
      for {
        (name, program) <- programs
        child <- program.children
      } yield child -> name

    programs.keys.find(!parents.contains(_)).get
  }

  def bottomProgram(programsStr: String): String = {
    bottomProgram(parsePrograms(programsStr))
  }

  def correctBalanceWeight(programs: Map[String, Program]): Int = {
    def helper(program: Program): Either[Int, Int] = {
      val childResults = program.children.map(programs).map(child => child.name -> helper(child)).toMap
      childResults.values.find(_.isLeft) match {
        case Some(left) => left
        case None =>
          val childTotalWeights = childResults.mapValues(_.right.get)
          val totalWeightChildren = program.children.groupBy(childTotalWeights)

          if (totalWeightChildren.size <= 1)
            Right(program.weight + childTotalWeights.values.sum) // no children or balanced
          else {
            require(totalWeightChildren.size == 2) // imbalanced

            val badChild = totalWeightChildren.find(_._2.size == 1).get._2.head // only one is imbalanced
            val badChildWeight = programs(badChild).weight
            val badChildTotalWeight = childTotalWeights(badChild)
            val goodTotalWeight = totalWeightChildren.find(_._2.size > 1).get._1 // imbalance is not ambiguous, more of other

            Left(goodTotalWeight - (badChildTotalWeight - badChildWeight))
          }
      }
    }

    val bottom = programs(bottomProgram(programs))
    helper(bottom).left.get
  }

  def correctBalanceWeight(programsStr: String): Int = {
    correctBalanceWeight(parsePrograms(programsStr))
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(bottomProgram(input))
    println(correctBalanceWeight(input))
  }
}
