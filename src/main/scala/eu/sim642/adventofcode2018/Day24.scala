package eu.sim642.adventofcode2018

import eu.sim642.adventofcodelib.IteratorImplicits._

object Day24 {

  sealed trait GroupType {
    def targetType: GroupType
  }
  case object ImmuneSystem extends GroupType {
    override def targetType: GroupType = Infection
  }
  case object Infection extends GroupType {
    override def targetType: GroupType = ImmuneSystem
  }

  case class Group(i: Int, groupType: GroupType, units: Int, unitHp: Int, attackDamage: Int, attackType: String, initiative: Int, weaknesses: Set[String], immunities: Set[String]) {

    def effectivePower: Int = units * attackDamage

    def damageTo(defendingGroup: Group): Int = {
      if (defendingGroup.immunities.contains(attackType))
        0
      else if (defendingGroup.weaknesses.contains(attackType))
        2 * effectivePower
      else
        effectivePower
    }

    //override def toString: String = s"$groupType $i"
  }

  def targetSelectionPhase(groups: Seq[Group]): Map[Group, Group] = {

    def helper(choosingGroups: List[Group], attackableGroups: Set[Group], targets: Map[Group, Group] = Map.empty): Map[Group, Group] = choosingGroups match {
      case Nil => targets
      case attackingGroup :: tl =>
        val enemyArmy = attackableGroups.filter(_.groupType == attackingGroup.groupType.targetType)
        if (enemyArmy.nonEmpty) {
          val target = enemyArmy.maxBy(defendingGroup => (attackingGroup.damageTo(defendingGroup), defendingGroup.effectivePower, defendingGroup.initiative))
          if (attackingGroup.damageTo(target) > 0)
            helper(tl, attackableGroups - target, targets + (attackingGroup -> target))
          else
            helper(tl, attackableGroups, targets)
        }
        else
          helper(tl, attackableGroups, targets)
    }

    val choosingGroups = groups.sortBy(group => (-group.effectivePower, -group.initiative))
    helper(choosingGroups.toList, groups.toSet)
  }

  def attackingPhase(groups: Seq[Group], targets: Map[Group, Group]): Seq[Group] = {

    def helper(attackingGroups: List[Group], targets: Map[Group, Group], groups: Set[Group]): Seq[Group] = attackingGroups match {
      case Nil => groups.toSeq
      case attackingGroup :: tl =>
        val defendingGroup = targets(attackingGroup)
        //print(s"$attackingGroup attacks $defendingGroup")
        if (groups.contains(defendingGroup)) { // is needed?
          val damage = attackingGroup.damageTo(defendingGroup)
          val killedUnits = damage / defendingGroup.unitHp
          //println(s" killing $killedUnits")
          val newDefendingGroup = defendingGroup.copy(units = defendingGroup.units - killedUnits)

          if (newDefendingGroup.units > 0) {
            val newGroups = groups - defendingGroup + newDefendingGroup
            val newTargets = if (targets.contains(defendingGroup)) targets - defendingGroup + (newDefendingGroup -> targets(defendingGroup)) else targets
            val newTl = tl.map({ tlGroup =>
              if (tlGroup == defendingGroup)
                newDefendingGroup
              else
                tlGroup
            })
            helper(newTl, newTargets, newGroups)
          }
          else {
            val newGroups = groups - defendingGroup
            val newTargets = targets - defendingGroup
            val newTl = tl.diff(List(defendingGroup))
            helper(newTl, newTargets, newGroups)
          }
        }
        else {
          //println()
          helper(tl, targets, groups)
        }
    }

    val attackingGroups = targets.keys.toSeq.sortBy(-_.initiative)
    helper(attackingGroups.toList, targets, groups.toSet)
  }

  def combat(initialGroups: Seq[Group]): Option[(GroupType, Int)] = {
    var groups = initialGroups
    while (groups.exists(_.groupType == ImmuneSystem) && groups.exists(_.groupType == Infection)) {
      //for (group <- groups)
      //  println(s"$group has ${group.units} units")

      val targets = targetSelectionPhase(groups)
      //for ((a, d) <- targets)
      //  println(s"$a targets $d")

      val newGroups = attackingPhase(groups, targets)
      if (newGroups == groups) {
        // infinitely stuck
        return None
      }
      groups = newGroups
      //println()
    }
    Some(groups.head.groupType, groups.map(_.units).sum)
  }

  def combatToWin(initialGroups: Seq[Group]): Int = combat(initialGroups).get._2

  def boostGroups(initialGroups: Seq[Group], boost: Int): Seq[Group] = {
    initialGroups.map({ group =>
      group.groupType match {
        case ImmuneSystem => group.copy(attackDamage = group.attackDamage + boost)
        case Infection => group
      }
    })
  }

  def smallestBoostedCombat(initialGroups: Seq[Group]): Int = {
    (for {
      boost <- Iterator.from(0)
      (ImmuneSystem, immuneUnits) <- combat(boostGroups(initialGroups, boost))
    } yield immuneUnits).head
  }


  private val weakImmuneRegex = """(weak|immune) to ([\w, ]+)""".r

  def parseWeakImmune(s: String): (Set[String], Set[String]) = {
    var weaknesses = Set.empty[String]
    var immunities = Set.empty[String]
    if (s != null) {
      val parts = s.replaceAll("""^\(""", "").replaceAll("""\) $""", "").split("; ")
      for (part <- parts) {
        part match {
          case weakImmuneRegex("weak", types) =>
            weaknesses = types.split(", ").toSet
          case weakImmuneRegex("immune", types) =>
            immunities = types.split(", ").toSet
        }
      }
    }
    (weaknesses, immunities)
  }

  private val groupRegex = """(\d+) units each with (\d+) hit points (\([^\)]+\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)""".r

  def parseGroup(s: String, i: Int, groupType: GroupType): Group = s match {
    case groupRegex(units, unitHp, weakImmune, attackDamage, attackType, initiative) =>
      val (weaknesses, immunities) = parseWeakImmune(weakImmune)
      Group(i, groupType, units.toInt, unitHp.toInt, attackDamage.toInt, attackType, initiative.toInt, weaknesses, immunities)
  }

  def parseInput(input: String): Seq[Group] = {
    val Seq("Immune System:" +: immuneLines, "Infection:" +: infectionLines) = input.split("""\n\n""").map(_.linesIterator.toList).toList

    immuneLines.zipWithIndex.map({ case (line, i) => parseGroup(line, i + 1, ImmuneSystem) }) ++
      infectionLines.zipWithIndex.map({case (line, i) => parseGroup(line, i + 1, Infection) })
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(combatToWin(parseInput(input)))
    println(smallestBoostedCombat(parseInput(input)))

    // 19293 - too low
  }
}
