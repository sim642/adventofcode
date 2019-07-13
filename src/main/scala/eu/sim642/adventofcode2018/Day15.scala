package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2017.Day14.PosGrid
import eu.sim642.adventofcode2017.Day19.Grid
import eu.sim642.adventofcode2017.Day3.Pos
import eu.sim642.adventofcode2018.Day2.HeadIterator

import scala.util.Try

object Day15 {

  sealed trait UnitType {
    def target: UnitType
  }

  case object Elf extends UnitType {
    override def target: UnitType = Goblin
  }

  case object Goblin extends UnitType {
    override def target: UnitType = Elf
  }

  case class CombatUnit(unitType: UnitType, pos: Pos, hp: Int = 200, attackPower: Int = 3)

  implicit val posReadingOrdering: Ordering[Pos] = Ordering.fromLessThan({ (pos1, pos2) =>
    pos1.y < pos2.y || (pos1.y == pos2.y && pos1.x < pos2.x)
  })

  implicit val combatUnitReadingOrdering: Ordering[CombatUnit] = Ordering.by(_.pos)

  def getTargets(unit: CombatUnit)(implicit units: List[CombatUnit]): Set[CombatUnit] = units.filter(_.unitType == unit.unitType.target).toSet

  def isFree(pos: Pos)(implicit grid: Grid[Char], units: List[CombatUnit]): Boolean = {
    grid(pos) == '.' && !units.exists(_.pos == pos)
  }

  def getInRange(targets: Set[CombatUnit])(implicit grid: Grid[Char], units: List[CombatUnit]): Set[Pos] = {
    for {
      target <- targets
      offset <- Pos.axisOffsets
      pos = target.pos + offset
      if isFree(pos)
    } yield pos
  }

  def bfs(startPos: Pos, endPos: Set[Pos])(implicit grid: Grid[Char], units: List[CombatUnit]): Map[Pos, Int] = {

    def helper(visited: Map[Pos, Int], toVisit: Map[Pos, Int]): Map[Pos, Int] = {
      val neighbors = for {
        (pos, dist) <- toVisit
        offset <- Pos.axisOffsets
        newPos = pos + offset
        if isFree(newPos)
      } yield newPos -> (dist + 1)
      val newVisited = visited ++ toVisit
      val newToVisit = neighbors -- visited.keys
      if (newToVisit.isEmpty || (toVisit.keySet intersect endPos).nonEmpty)
        newVisited
      else
        helper(newVisited, newToVisit)
    }

    helper(Map.empty, Map(startPos -> 0))
  }

  def getReachable(unit: CombatUnit, inRange: Set[Pos])(implicit grid: Grid[Char], units: List[CombatUnit]): Map[Pos, Int] = {
    bfs(unit.pos, inRange).view.filterKeys(inRange).toMap
  }

  def getNearest(reachable: Map[Pos, Int]): Set[Pos] = {
    val minDist = reachable.values.min
    reachable.filter(_._2 == minDist).keySet
  }

  def getChosen(nearest: Set[Pos]): Pos = nearest.min

  def getStep(chosen: Pos, unit: CombatUnit)(implicit grid: Grid[Char], units: List[CombatUnit]): Pos = {
    val unitNeighbors = Pos.axisOffsets.map(unit.pos + _).toSet
    val bfsMap = bfs(chosen, unitNeighbors)
    val neighborDists = bfsMap.view.filterKeys(unitNeighbors).toMap
    val minDist = neighborDists.values.min
    neighborDists.filter(_._2 == minDist).keys.min
  }

  class ElfDeathException extends RuntimeException

  def simulateCombat(grid: Grid[Char], units: List[CombatUnit], elfDeath: Boolean = false): (Int, List[CombatUnit]) = {

    def round(units: List[CombatUnit]): (List[CombatUnit], Boolean) = {

      def turn(init: List[CombatUnit], tail: List[CombatUnit], done: Boolean): (List[CombatUnit], Boolean) = tail match {
        case Nil => (init, done)
        case unit :: tl =>
          implicit val otherUnits: List[CombatUnit] = init ++ tl
          implicit val implicitGrid: Grid[Char] = grid
          val targets = getTargets(unit)
          val done2 = done || targets.isEmpty
          val inRange = getInRange(targets)

          var unit2 = unit
          if (!inRange.contains(unit.pos)) {
            val reachable = getReachable(unit, inRange)
            if (reachable.nonEmpty) {
              val nearest = getNearest(reachable)
              val chosen = getChosen(nearest)
              val step = getStep(chosen, unit)
              unit2 = unit.copy(pos = step)
            }
          }

          val unitNeighbors = Pos.axisOffsets.map(unit2.pos + _).toSet
          val attackUnits = getTargets(unit2).filter(u => unitNeighbors.contains(u.pos))

          if (attackUnits.nonEmpty) {
            val attackUnit = attackUnits.minBy(u => (u.hp, u.pos))
            val attackUnit2 = attackUnit.copy(hp = attackUnit.hp - unit2.attackPower)
            val attackUnit3 = if (attackUnit2.hp > 0) Some(attackUnit2) else None
            if (elfDeath && attackUnit3.isEmpty && attackUnit2.unitType == Elf)
              throw new ElfDeathException

            val init2 = init.flatMap(u => if (u == attackUnit) attackUnit3 else Some(u))
            val tl2 = tl.flatMap(u => if (u == attackUnit) attackUnit3 else Some(u))
            turn(unit2 :: init2, tl2, done2)
          }
          else {
            turn(unit2 :: init, tl, done2)
          }
      }

      turn(Nil, units.sorted, done = false)
    }

    val roundIt = Iterator.iterate((units, false))({ case (units, _) => round(units) })
    val ((finalUnits, _), finalI) = roundIt.zipWithIndex.find({ case ((_, done), _) => done }).get

    (finalI - 1, finalUnits)
  }

  def combatOutcome(grid: Grid[Char], units: List[CombatUnit]): Int = {
    val (fullRounds, finalUnits) = simulateCombat(grid, units)
    val hpSum = finalUnits.map(_.hp).sum
    fullRounds * hpSum
  }

  def combatOutcomeElfWin(grid: Grid[Char], units: List[CombatUnit]): Int = {
    def withElfAttackPower(elfAttackPower: Int): Option[Int] = {
      println(s"Elf attack power: $elfAttackPower")

      val newUnits = units.map({
        case unit@CombatUnit(Elf, _, _, _) => unit.copy(attackPower = elfAttackPower)
        case unit => unit
      })

      Try(simulateCombat(grid, newUnits, elfDeath = true)).toOption.map({ case (fullRounds, finalUnits) =>
        val hpSum = finalUnits.map(_.hp).sum
        fullRounds * hpSum
      })
    }

    val attackPowers = (4 to 200).groupBy(e => math.ceil(200.0 / e).toInt).mapValues(_.min).values.toStream.sorted // 20 and 21 both require 10 hits to kill goblin, no point in trying both
    attackPowers.flatMap(withElfAttackPower).head // Stream makes flatMapping lazy
  }

  def combatOutcome(input: String): Int = {
    val (grid, units) = parseInput(input)
    combatOutcome(grid, units)
  }

  def combatOutcomeElfWin(input: String): Int = {
    val (grid, units) = parseInput(input)
    combatOutcomeElfWin(grid, units)
  }


  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  def parseUnit(cell: Char, pos: Pos): Option[CombatUnit] = {
    val unitType: Option[UnitType] = cell match {
      case 'E' => Some(Elf)
      case 'G' => Some(Goblin)
      case _ => None
    }

    unitType.map(CombatUnit(_, pos))
  }

  def parseInput(input: String): (Grid[Char], List[CombatUnit]) = {
    val units: List[CombatUnit] = (for {
      (row, y) <- parseGrid(input).zipWithIndex
      (cell, x) <- row.zipWithIndex
      pos = Pos(x, y)
      unit <- parseUnit(cell, pos)
    } yield unit).toList

    val grid = parseGrid(input.replace('E', '.').replace('G', '.'))

    (grid, units)
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day15.txt")).mkString.stripLineEnd

  def main(args: Array[String]): Unit = {
    println(combatOutcome(input))
    println(combatOutcomeElfWin(input))
  }
}
