package eu.sim642.adventofcode2022

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.util.parsing.combinator.RegexParsers

object Day19 extends RegexParsers {

  enum Resource {
    case Ore
    case Clay
    case Obsidian
    case Geode
  }

  case class Blueprint(robotCosts: Map[Resource, Map[Resource, Int]])

  def maxGeodes(blueprint: Blueprint): Int = {

    case class State(robots: ArraySeq[Int],
                     resources: ArraySeq[Int]) {

      def nexts: IterableOnce[State] = {
        val newRobotStates = for {
          //(robot, costs) <- blueprint.robotCosts.iterator
          robot <- Iterator(Resource.Geode, Resource.Obsidian, Resource.Clay, Resource.Ore)
          costs = blueprint.robotCosts(robot)
          if costs.forall({ case (resource, amount) => resources(resource.ordinal) >= amount })
          newRobots = robots.updated(robot.ordinal, robots(robot.ordinal) + 1)
          newResources = costs.foldLeft(resources)({ case (acc, (resource, amount)) => acc.updated(resource.ordinal, acc(resource.ordinal) - amount) })
        } yield State(newRobots, newResources)

        def collectResources(state: State): State = {
          val newResources = robots.zipWithIndex.foldLeft(state.resources)({ case (acc, (amount, robot)) => acc.updated(robot, acc(robot) + amount) })
          state.copy(resources = newResources)
        }

        /*if (newRobotStates.size == 4)
          newRobotStates.map(collectResources)
        else*/
          (newRobotStates.take(2) ++ Iterator.single(this)).map(collectResources)
      }
    }

    @tailrec
    def helper(minute: Int, states: Set[State]): Set[State] = {
      println(s"$minute: ${states.size}")
      if (minute >= 24)
        states
      else {
        val newStates = states.flatMap(_.nexts)
        helper(minute + 1, newStates)
      }
    }

    val initialStates = Set(State(ArraySeq(1, 0, 0, 0), ArraySeq(0, 0, 0, 0)))
    val finalStates = helper(0, initialStates)
    finalStates
      .view
      .map(_.resources(3))
      .max
  }

  def sumQualityLevel(blueprints: Seq[Blueprint]): Int = {
    blueprints
      .view
      .zipWithIndex
      .map({ case (blueprint, i) => (i + 1) * maxGeodes(blueprint) })
      .sum
  }


  def parseBlueprint(s: String): Blueprint = {

    def resource: Parser[Resource] = (
      "ore" ^^^ Resource.Ore
    | "clay" ^^^ Resource.Clay
    | "obsidian" ^^^ Resource.Obsidian
    | "geode" ^^^ Resource.Geode
    )

    def cost: Parser[(Resource, Int)] =
      "\\d+".r ~ resource ^^ { case amount ~ resource => resource -> amount.toInt }

    def costs: Parser[Map[Resource, Int]] = (
      cost ~ "and" ~ cost ^^ { case cost1 ~ _ ~ cost2 => Map(cost1, cost2) }
    | cost ^^ { cost => Map(cost) }
    )

    def robotCost: Parser[(Resource, Map[Resource, Int])] =
      "Each" ~> resource ~ "robot costs" ~ costs <~ "." ^^ { case resource ~ _ ~ costs => resource -> costs }

    def blueprint: Parser[Blueprint] =
      "Blueprint \\d+:".r ~> rep(robotCost) ^^ { robotCosts => Blueprint(robotCosts.toMap) }

    parseAll(blueprint, s) match {
      case Success(r, _) => r
      case NoSuccess(s2, t) => throw new IllegalArgumentException(s"$s, $s2: ${t.toString}")
    }
  }

  def parseBlueprints(input: String): Seq[Blueprint] = input.linesIterator.map(parseBlueprint).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val exampleInput =
      """Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
        |Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.""".stripMargin


    //println(sumQualityLevel(parseBlueprints(exampleInput)))
    println(sumQualityLevel(parseBlueprints(input)))

    // part 1: 1310 - too low (greedily make best robot)
  }
}
