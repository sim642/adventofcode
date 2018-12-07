package eu.sim642.adventofcode2018

object Day7 {

  type Step = Char
  type Requirements = Map[Step, Set[Step]]

  def topologicalSort(reqs: Requirements): String = {
    reqs.values.reduceOption(_ ++ _) match {
      case None => ""
      case Some(haveReqStep) =>
        val step = (reqs.keySet -- haveReqStep).min
        val restReqs = reqs.filterKeys(_ != step)
        step + topologicalSort(restReqs)
    }
  }


  private val requirementRegex = """Step ([A-Z]) must be finished before step ([A-Z]) can begin.""".r

  def parseRequirement(s: String): (Step, Step) = s match {
    case requirementRegex(reqStep, step) => reqStep.head -> step.head
  }

  def parseRequirements(input: String): Requirements = {
    input.lines.map(parseRequirement).foldLeft(Map.empty[Step, Set[Step]])({
      case (reqs, (reqStep, step)) =>
        reqs +
          (reqStep -> (reqs.getOrElse(reqStep, Set()) + step)) +
          (step -> reqs.getOrElse(step, Set()))
    })
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(topologicalSort(parseRequirements(input)))
  }
}
