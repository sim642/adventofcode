package eu.sim642.adventofcode2018

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

  def combat(initialGroups: Seq[Group]): Int = {
    var groups = initialGroups
    while (groups.exists(_.groupType == ImmuneSystem) && groups.exists(_.groupType == Infection)) {
      //for (group <- groups)
      //  println(s"$group has ${group.units} units")

      val targets = targetSelectionPhase(groups)
      //for ((a, d) <- targets)
      //  println(s"$a targets $d")

      groups = attackingPhase(groups, targets)
      //println()
    }
    groups.map(_.units).sum
  }

  def boostGroups(initialGroups: Seq[Group], boost: Int): Seq[Group] = {
    initialGroups.map({ group =>
      group.groupType match {
        case ImmuneSystem => group.copy(attackDamage = group.attackDamage + boost)
        case Infection => group
      }
    })
  }

  def boostedCombat(initialGroups: Seq[Group], boost: Int): Option[(GroupType, Int)] = {
    var groups = boostGroups(initialGroups, boost)
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

  def smallestBoostedCombat(initialGroups: Seq[Group]): Int = {
    for (boost <- Iterator.from(0)) {
      //println(s"Boost: $boost")
      boostedCombat(initialGroups, boost) match {
        case Some((ImmuneSystem, immuneUnits)) =>
          return immuneUnits
        case _ =>
      }
    }
    ???
  }

  // TODO: combinator parsing...
  private val groupWeakImmuneRegex = """(\d+) units each with (\d+) hit points \(weak to ([\w, ]+); immune to ([\w, ]+)\) with an attack that does (\d+) (\w+) damage at initiative (\d+)""".r
  private val groupImmuneWeakRegex = """(\d+) units each with (\d+) hit points \(immune to ([\w, ]+); weak to ([\w, ]+)\) with an attack that does (\d+) (\w+) damage at initiative (\d+)""".r
  private val groupWeakRegex = """(\d+) units each with (\d+) hit points \(weak to ([\w, ]+)\) with an attack that does (\d+) (\w+) damage at initiative (\d+)""".r
  private val groupImmuneRegex = """(\d+) units each with (\d+) hit points \(immune to ([\w, ]+)\) with an attack that does (\d+) (\w+) damage at initiative (\d+)""".r
  private val groupRegex = """(\d+) units each with (\d+) hit points with an attack that does (\d+) (\w+) damage at initiative (\d+)""".r

  def parseGroup(s: String, i: Int, groupType: GroupType): Group = s match {
    case groupWeakImmuneRegex(units, unitHp, weaknesses, immunities, attackDamage, attackType, initiative) =>
      Group(i, groupType, units.toInt, unitHp.toInt, attackDamage.toInt, attackType, initiative.toInt, weaknesses.split(", ").toSet, immunities.split(", ").toSet)
    case groupImmuneWeakRegex(units, unitHp, immunities, weaknesses, attackDamage, attackType, initiative) =>
      Group(i, groupType, units.toInt, unitHp.toInt, attackDamage.toInt, attackType, initiative.toInt, weaknesses.split(", ").toSet, immunities.split(", ").toSet)
    case groupWeakRegex(units, unitHp, weaknesses, attackDamage, attackType, initiative) =>
      Group(i, groupType, units.toInt, unitHp.toInt, attackDamage.toInt, attackType, initiative.toInt, weaknesses.split(", ").toSet, Set.empty)
    case groupImmuneRegex(units, unitHp, immunities, attackDamage, attackType, initiative) =>
      Group(i, groupType, units.toInt, unitHp.toInt, attackDamage.toInt, attackType, initiative.toInt, Set.empty, immunities.split(", ").toSet)
    case groupRegex(units, unitHp, attackDamage, attackType, initiative) =>
      Group(i, groupType, units.toInt, unitHp.toInt, attackDamage.toInt, attackType, initiative.toInt, Set.empty, Set.empty)
  }

  def parseInput(input: String): Seq[Group] = {
    val Seq("Immune System:" +: immuneLines, "Infection:" +: infectionLines) = input.split("""\n\n""").map(_.lines.toList).toList

    immuneLines.zipWithIndex.map(p => parseGroup(p._1, p._2 + 1, ImmuneSystem)) ++ infectionLines.zipWithIndex.map(p => parseGroup(p._1, p._2 + 1, Infection))
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(combat(parseInput(input)))
    println(smallestBoostedCombat(parseInput(input)))

    // 19293 - too low
  }
}
