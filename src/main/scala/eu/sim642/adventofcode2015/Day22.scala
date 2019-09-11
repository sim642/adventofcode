package eu.sim642.adventofcode2015

import eu.sim642.adventofcodelib.graph.{Dijkstra, GraphSearch}

object Day22 {

  case class State(myHitpoints: Int, myArmor: Int = 0,
                   myMana: Int, myManaUsed: Int = 0,
                   enemyHitpoints: Int, enemyDamage: Int,
                   effects: Map[Effect, Int] = Map.empty) {

    def myAlive: Boolean = myHitpoints > 0
    def enemyAlive: Boolean = enemyHitpoints > 0

    def effectsApplied: State = effects.keys.foldLeft(this)({ case (state, effect) => effect.apply(state) })

    def myTurns: Seq[State] = allSpells.flatMap(_.cast(this))

    def enemyTurn: Option[State] = {
      if (enemyAlive) {
        val damageDealt = (enemyDamage - myArmor) max 1
        Some(copy(myHitpoints = myHitpoints - damageDealt))
          .filter(_.myAlive)
      }
      else
        Some(this).filter(_.myAlive)
    }

    def turns: Seq[State] = {
      assert(myAlive)
      if (enemyAlive) {
        val afterEffects = effectsApplied
        if (afterEffects.myAlive)
          afterEffects.myTurns.flatMap(_.effectsApplied.enemyTurn)
        else
          Seq.empty
      }
      else
        Seq.empty
    }
  }

  sealed trait Spell {
    val manaCost: Int
    protected def castInner(state: State): Option[State]

    def cast(state: State): Option[State] = {
      if (state.myMana >= manaCost) {
        val newState = state.copy(myMana = state.myMana - manaCost,
                                  myManaUsed = state.myManaUsed + manaCost)
        castInner(newState)
      }
      else
        None
    }
  }
  case object MagicMissle extends Spell {
    override val manaCost: Int = 53
    override protected def castInner(state: State): Option[State] = {
      Some(state.copy(enemyHitpoints = state.enemyHitpoints - 4))
    }

  }
  case object Drain extends Spell {
    override val manaCost: Int = 73
    override protected def castInner(state: State): Option[State] = {
      Some(state.copy(myHitpoints = state.myHitpoints + 2,
                      enemyHitpoints = state.enemyHitpoints - 2))
    }
  }

  sealed trait Effect extends Spell {
    val turns: Int

    override protected def castInner(state: State): Option[State] = {
      if (!state.effects.contains(this))
        Some(state.copy(effects = state.effects + (this -> turns)))
      else
        None
    }

    protected def applyInner(state: State): State

    def apply(state: State): State = {
      val newState = applyInner(state)
      val thisTurns = newState.effects(this)
      newState.copy(effects = {
        if (thisTurns > 1)
          newState.effects + (this -> (thisTurns - 1))
        else
          newState.effects - this
      })
    }
  }
  case object Shield extends Effect {
    override val manaCost: Int = 113
    override val turns: Int = 6

    override protected def castInner(state: State): Option[State] = {
      super.castInner(state).map({ state =>
        state.copy(myArmor = state.myArmor + 7)
      })
    }

    override protected def applyInner(state: State): State = {
      if (state.effects(this) == 1) // last turn
        state.copy(myArmor = state.myArmor - 7)
      else
        state
    }
  }
  case object Poison extends Effect {
    override val turns: Int = 6
    override val manaCost: Int = 173

    override protected def applyInner(state: State): State = {
      state.copy(enemyHitpoints = state.enemyHitpoints - 3)
    }
  }
  case object Recharge extends Effect {
    override val turns: Int = 5
    override val manaCost: Int = 229

    override protected def applyInner(state: State): State = {
      state.copy(myMana = state.myMana + 101)
    }
  }

  case object Hard extends Effect {
    override val turns: Int = Int.MaxValue // hack
    override val manaCost: Int = 0 // unused

    override protected def applyInner(state: State): State = {
      state.copy(myHitpoints = state.myHitpoints - 1)
    }
  }

  private val allSpells: Seq[Spell] = Seq(MagicMissle, Drain, Shield, Poison, Recharge)

  def leastWinManaUsed(initialState: State): Int = {
    val graphSearch = new GraphSearch[State] {
      override val startNode: State = initialState

      override def neighbors(state: State): IterableOnce[(State, Int)] = {
        for {
          newState <- state.turns
        } yield newState -> (newState.myManaUsed - state.myManaUsed)
      }

      override def isTargetNode(state: State, dist: Int): Boolean = !state.enemyAlive
    }

    Dijkstra.search(graphSearch).target.get._2
  }

  def leastWinManaUsed(input: String): Int = leastWinManaUsed(parseInitialState(input))

  def leastWinManaUsedHard(initialState: State): Int = {
    val newInitialState = Hard.cast(initialState).get // manually cast spell, can't fail
    leastWinManaUsed(newInitialState)
  }

  def leastWinManaUsedHard(input: String): Int = leastWinManaUsedHard(parseInitialState(input))


  private val inputRegex =
    """Hit Points: (\d+)
      |Damage: (\d+)""".stripMargin.r

  def parseInitialState(input: String): State = input match {
    case inputRegex(hitpoints, damage) => State(myHitpoints = 50, myMana = 500,
                                                enemyHitpoints = hitpoints.toInt,
                                                enemyDamage = damage.toInt)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day22.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(leastWinManaUsed(input))
    println(leastWinManaUsedHard(input))
  }
}
