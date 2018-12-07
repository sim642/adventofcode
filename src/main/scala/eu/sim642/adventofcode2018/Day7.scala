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

  def parallelTopologicalSort(reqs: Requirements, workerCount: Int = 5, baseStepTime: Int = 60): Int = {

    case class Work(step: Step, timeLeft: Int)

    def helper(reqs: Requirements, workers: Seq[Option[Work]], inProgress: Set[Step]): Int = {
      def pickWork(reqs: Requirements, workers: Seq[Option[Work]], inProgress: Set[Step]) = {
        reqs.values.reduceOption(_ ++ _) match {
          case None => (reqs, workers :+ None, inProgress)
          case Some(haveReqStep) =>
            val possibleSteps = reqs.keySet -- haveReqStep -- inProgress
            if (possibleSteps.nonEmpty) {
              val step = possibleSteps.min
              (reqs, workers :+ Some(Work(step, baseStepTime + (step.toInt - 'A'.toInt + 1) - 1)), inProgress + step)
            }
            else
              (reqs, workers :+ None, inProgress)
        }
      }

      println(s"1: $workers $inProgress")

      val (reqs2, workers2, inProgress2) = workers.foldLeft((reqs, Seq.empty[Option[Work]], inProgress))({
        case ((reqs, workers, inProgress), None) =>
          (reqs, workers :+ None, inProgress)

        case ((reqs, workers, inProgress), Some(Work(step, timeLeft))) =>
          if (timeLeft == 0) {
            val restReqs = reqs.filterKeys(_ != step)
            (restReqs, workers :+ None, inProgress - step)
          }
          else
            (reqs, workers :+ Some(Work(step, timeLeft - 1)), inProgress)
      })

      println(s"2: $workers2 $inProgress2")

      val (reqs3, workers3, inProgress3) = workers2.foldLeft((reqs2, Seq.empty[Option[Work]], inProgress2))({
        case ((reqs, workers, inProgress), None) =>
          pickWork(reqs, workers, inProgress)

        case ((reqs, workers, inProgress), s@Some(Work(step, timeLeft))) =>
          (reqs, workers :+ s, inProgress)
      })

      println(s"3: $workers3 $inProgress3")

      if (reqs3.isEmpty && inProgress3.isEmpty)
        return 0

      1 + helper(reqs3, workers3, inProgress3)
    }

    helper(reqs, Seq.fill(workerCount)(None), Set.empty)
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
    println(parallelTopologicalSort(parseRequirements(input)))

    // 894 - too high
  }
}
