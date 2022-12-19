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

  def maxGeodes(blueprint: Blueprint, maxMinutes: Int): Int = {

    val robotCosts2 = {
      Resource.values.map({ r =>
        val c = blueprint.robotCosts(r)
        Resource.values.map(r2 => c.getOrElse(r2, 0)).to(ArraySeq)
      }).to(ArraySeq)
    }

    val maxResourceCosts = robotCosts2.transpose.map(_.max).updated(3, Int.MaxValue)

    case class State(robots: ArraySeq[Int],
                     resources: ArraySeq[Int]) {

      def nexts(minute: Int): IterableOnce[State] = {
        val newRobotStates = for {
          //(robot, costs) <- blueprint.robotCosts.iterator
          robot <- (0 to 3).reverseIterator
          //if robot != 0 || resources.head <= maxOreCost
          if robots(robot) < maxResourceCosts(robot)
          costs = robotCosts2(robot)
          //if costs.zipWithIndex.forall({ case (amount, resource) => resources(resource) >= amount })
          if resources.lazyZip(costs).forall(_ >= _)
          newRobots = robots.updated(robot, robots(robot) + 1)
          //newResources = costs.zipWithIndex.foldLeft(resources)({ case (acc, (amount, resource)) => acc.updated(resource, acc(resource) - amount) })
          newResources = resources.lazyZip(costs).map(_ - _)
        } yield State(newRobots, newResources)

        def collectResources(state: State): State = {
          //val newResources = robots.zipWithIndex.foldLeft(state.resources)({ case (acc, (amount, robot)) => acc.updated(robot, acc(robot) + amount) })
          val newResources = state.resources.lazyZip(robots).map(_ + _)
          val newResources2 = newResources.lazyZip(maxResourceCosts).map(_ min (maxMinutes - minute) * _).updated(3, newResources(3))
          state.copy(resources = newResources2)
        }

        /*if (newRobotStates.size == 4)
          newRobotStates.map(collectResources)
        else*/
          (newRobotStates.take(2) ++ Iterator.single(this)).map(collectResources)
          //(newRobotStates.take(2) ++ (if (resources.head < maxOreCost) Iterator.single(this) else Iterator.empty)).map(collectResources)
      }
    }

    @tailrec
    def helper(minute: Int, states: Set[State]): Set[State] = {
      println(s"$minute: ${states.size}")
      if (minute >= maxMinutes)
        states
      else {
        val newStates = states.flatMap(_.nexts(minute))
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
      .map({ case (blueprint, i) => (i + 1) * maxGeodes(blueprint, 24) })
      .sum
  }

  def productMaxGeodes(blueprints: Seq[Blueprint]): Long = {
    blueprints
      .view
      .take(3)
      .map(maxGeodes(_, 32).toLong)
      .product
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
    //println(sumQualityLevel(parseBlueprints(input)))
    println(productMaxGeodes(parseBlueprints(input)))

    // part 1: 1310 - too low (greedily make best robot)
  }
}
