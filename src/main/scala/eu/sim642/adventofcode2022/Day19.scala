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

  def triangular(n: Int): Int = (n + 1) * n / 2

  def maxGeodes(blueprint: Blueprint, maxMinutes: Int): Int = {

    // use arrays with resources in order

    val robotCostsIndexed =
      Resource.values.map({ robot =>
        val costs = blueprint.robotCosts(robot)
        Resource.values.map(resource => costs(resource)).to(ArraySeq)
      }).to(ArraySeq)

    val geodeIndex = Resource.Geode.ordinal
    val maxResourceCosts = robotCostsIndexed.transpose.map(_.max).updated(geodeIndex, Int.MaxValue) // no geode limit

    case class State(minutesRemaining: Int,
                     robots: ArraySeq[Int],
                     resources: ArraySeq[Int])
                    (val prevRobots: ArraySeq[Int],
                     val prevResources: ArraySeq[Int]) {

      /**
       * Can construct robot with resources?
       */
      private def canConstruct(robot: Int): Boolean = {
        val costs = robotCostsIndexed(robot)
        resources.lazyZip(costs).forall(_ >= _)
      }

      /**
       * Should construct robot?
       * For pruning.
       */
      private def shouldConstruct(robot: Int): Boolean = {
        val costs = robotCostsIndexed(robot)
        robots(robot) < maxResourceCosts(robot) && // no need for more robots, already enough to instantly generate all possible needs
          (robots != prevRobots || // previously built some robot, don't prune
            !prevResources.lazyZip(costs).forall(_ >= _)) // don't build robot, which could've been previously built already
      }

      private def construct(robot: Int): State = {
        val costs = robotCostsIndexed(robot)
        val newRobots = robots.updated(robot, robots(robot) + 1)
        val newResources = resources.lazyZip(costs).map(_ - _)
        State(minutesRemaining, newRobots, newResources)(robots, resources)
      }

      private def noop: State = copy()(robots, resources)

      private def collectResources(collectRobots: ArraySeq[Int]): State = {
        val newResources = resources.lazyZip(collectRobots).map(_ + _) // resources from robots, which existed in the beginning of the step
        val newResources2 = newResources.lazyZip(maxResourceCosts).lazyZip(robots).map({ (resource, maxCost, robots) =>
          val maxRemainingNeed = minutesRemaining * maxCost
          val minRemainingCollect = robots * (minutesRemaining - 1)
          resource min (maxRemainingNeed - minRemainingCollect) // discard resources that cannot get used, prunes by deduplicating
        }).updated(geodeIndex, newResources(geodeIndex)) // don't prune geodes
        copy(resources = newResources2, minutesRemaining = minutesRemaining - 1)(robots, resources)
      }

      /**
       * Can reach geode count in remaining time?
       * For pruning.
       */
      private def canReachGeodes(geodes: Int): Boolean = {
        val currentGeodes = resources(geodeIndex)
        val currentRobotGeodes = robots(geodeIndex) * minutesRemaining
        val newRobotGeodes = triangular(minutesRemaining) // every remaining minute creates a new geode robot and they all collect geodes
        currentGeodes + currentRobotGeodes + newRobotGeodes >= geodes
      }

      def steps: IterableOnce[State] = {
        val newStates =
          if (canConstruct(geodeIndex)) // prefer geode robot if possible
            List(construct(geodeIndex))
          else {
            val newRobotStates = (for {
              robot <- (0 until geodeIndex).iterator
              if canConstruct(robot) && shouldConstruct(robot)
            } yield construct(robot)).toList
            noop :: newRobotStates
          }
        val newStates2 = newStates.map(_.collectResources(robots))

        val maxGeodes = newStates2.view.map(_.resources(geodeIndex)).max
        newStates2.filter(_.canReachGeodes(maxGeodes)) // prune states, which cannot beat best
      }
    }

    @tailrec
    def helper(states: Set[State]): Set[State] = {
      val minutesRemaining = states.head.minutesRemaining // all states have same
      //println(s"${maxMinutes - minutesRemaining}: ${states.size}")
      if (minutesRemaining <= 0)
        states
      else
        helper(states.flatMap(_.steps))
    }

    val initialStates = Set(State(maxMinutes, ArraySeq(1, 0, 0, 0), ArraySeq(0, 0, 0, 0))(ArraySeq(1, 0, 0, 0), ArraySeq(0, 0, 0, 0)))
    val finalStates = helper(initialStates)
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
      "Each" ~> resource ~ "robot costs" ~ costs <~ "." ^^ { case resource ~ _ ~ costs => resource -> costs.withDefaultValue(0) }

    def blueprint: Parser[Blueprint] =
      "Blueprint \\d+:".r ~> rep(robotCost) ^^ { robotCosts => Blueprint(robotCosts.toMap) }

    parseAll(blueprint, s).get
  }

  def parseBlueprints(input: String): Seq[Blueprint] = input.linesIterator.map(parseBlueprint).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumQualityLevel(parseBlueprints(input)))
    println(productMaxGeodes(parseBlueprints(input)))

    // part 1: 1310 - too low (greedily make best robot)
  }
}
