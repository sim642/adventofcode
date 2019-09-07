package eu.sim642.adventofcode2015

object Day21 {

  case class Item(cost: Int, damage: Int, armor: Int) {
    def +(that: Item): Item =
      Item(cost + that.cost, damage + that.damage, armor + that.armor)
  }

  private val weapons = Seq(
    Item(8, 4, 0),
    Item(10, 5, 0),
    Item(25, 6, 0),
    Item(40, 7, 0),
    Item(74, 8, 0),
  )

  private val armor = Seq(
    Item(13, 0, 1),
    Item(31, 0, 2),
    Item(53, 0, 3),
    Item(75, 0, 4),
    Item(102, 0, 5),
  )

  private val rings = Seq(
    Item(25, 1, 0),
    Item(50, 2, 0),
    Item(100, 3, 0),
    Item(20, 0, 1),
    Item(40, 0, 2),
    Item(80, 0, 3),
  )

  def iterateItemCombinations(): Iterator[Seq[Item]] = {
    for {
      myWeapons <- weapons.iterator.map(Seq(_))
      myArmors <- Iterator(Seq()) ++ armor.iterator.map(Seq(_))
      myRings <- Iterator(Seq()) ++ rings.iterator.map(Seq(_)) ++ rings.combinations(2)
    } yield myWeapons ++ myArmors ++ myRings
  }

  def turnsToKill(myDamage: Int, enemyArmor: Int, enemyHitpoints: Int): Int = {
    val damageDealt = (myDamage - enemyArmor) max 1
    (enemyHitpoints.toFloat / damageDealt).ceil.toInt
  }

  private val myHitpoints = 100

  def leastWinGold(enemyDamage: Int, enemyArmor: Int, enemyHitpoints: Int): Int = {
    def wouldIWin(totalItem: Item): Boolean = {
      turnsToKill(totalItem.damage, enemyArmor, enemyHitpoints) <= turnsToKill(enemyDamage, totalItem.armor, myHitpoints)
    }

    iterateItemCombinations()
      .map(_.reduce(_ + _))
      .filter(wouldIWin)
      .minBy(_.cost)
      .cost
  }

  def leastWinGold(input: String): Int = {
    val (enemyDamage, enemyArmor, enemyHitpoints) = parseEnemy(input)
    leastWinGold(enemyDamage, enemyArmor, enemyHitpoints)
  }

  // TODO: reduce duplication
  def mostLoseGold(enemyDamage: Int, enemyArmor: Int, enemyHitpoints: Int): Int = {
    def wouldIWin(totalItem: Item): Boolean = {
      turnsToKill(totalItem.damage, enemyArmor, enemyHitpoints) <= turnsToKill(enemyDamage, totalItem.armor, myHitpoints)
    }

    iterateItemCombinations()
      .map(_.reduce(_ + _))
      .filter(!wouldIWin(_))
      .maxBy(_.cost)
      .cost
  }

  def mostLoseGold(input: String): Int = {
    val (enemyDamage, enemyArmor, enemyHitpoints) = parseEnemy(input)
    mostLoseGold(enemyDamage, enemyArmor, enemyHitpoints)
  }


  private val inputRegex =
    """Hit Points: (\d+)
      |Damage: (\d+)
      |Armor: (\d+)""".stripMargin.r

  def parseEnemy(input: String): (Int, Int, Int) = input match {
    case inputRegex(hitpoints, damage, armor) => (damage.toInt, armor.toInt, hitpoints.toInt)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(leastWinGold(input))
    println(mostLoseGold(input))
  }
}
