package eu.sim642.adventofcode2018

import scala.collection.immutable.SortedSet

object Day7 {

  type Step = Char
  type Requirements = Map[Step, Set[Step]]

  def topologicalSort(reqs: Requirements, revOrder: List[Step] = Nil): String = {
    reqs.values.reduceOption(_ ++ _) match {
      case None => revOrder.reverse.mkString("")
      case Some(haveReqStep) =>
        val step = (reqs.keySet -- haveReqStep).min
        val restReqs = reqs.view.filterKeys(_ != step).toMap
        topologicalSort(restReqs, step :: revOrder)
    }
  }

  def parallelTopologicalSort(reqs: Requirements, workerCount: Int = 5, baseStepTime: Int = 60): Int = {

    case class Work(step: Step, timeLeft: Int) {
      def ticked(time: Int): Work = copy(timeLeft = timeLeft - time)
    }

    object Work {
      def apply(step: Step): Work = Work(step, baseStepTime + (step - 'A' + 1))
    }

    def tickTime(reqs: Requirements, works: Set[Work]) = {
      val tick = works.map(_.timeLeft).min // tick until first work finishes, nothing interesting happens in between anyway
      val tickedWorks = works.map(_.ticked(tick))

      val (endWorks, restWorks) = tickedWorks.partition(_.timeLeft == 0)
      val endSteps = endWorks.map(_.step)
      val restReqs = reqs -- endSteps

      (restReqs, restWorks, tick)
    }

    def pickNewWorks(reqs: Requirements, works: Set[Work]): Set[Work] = {
      reqs.values.reduceOption(_ ++ _) match {
        case None => Set.empty
        case Some(haveReqStep) =>
          val inProgress = works.map(_.step)
          val possibleSteps = reqs.keySet -- haveReqStep -- inProgress
          val newSteps = possibleSteps.to(SortedSet).take(workerCount - works.size)
          val newWorks = newSteps.unsorted.map(Work(_))
          newWorks
      }
    }

    def helper(reqs: Requirements, works: Set[Work] = Set.empty, time: Int = 0): Int = {
      if (reqs.isEmpty && works.isEmpty)
        return time

      val works2 = works ++ pickNewWorks(reqs, works)
      val (reqs2, works3, tick) = tickTime(reqs, works2)
      helper(reqs2, works3, time + tick)
    }

    helper(reqs)
  }


  private val requirementRegex = """Step ([A-Z]) must be finished before step ([A-Z]) can begin.""".r

  def parseRequirement(s: String): (Step, Step) = s match {
    case requirementRegex(reqStep, step) => reqStep.head -> step.head
  }

  def parseRequirements(input: String): Requirements = {
    input.linesIterator.map(parseRequirement).foldLeft(Map.empty[Step, Set[Step]])({
      case (reqs, (reqStep, step)) =>
        reqs +
          (reqStep -> (reqs.getOrElse(reqStep, Set()) + step)) +
          (step -> reqs.getOrElse(step, Set()))
    })
  }


  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(topologicalSort(parseRequirements(input)))
    println(parallelTopologicalSort(parseRequirements(input)))

    // 894 - too high
  }
}
