package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2017.Day19.Grid
import eu.sim642.adventofcode2017.Day3.Pos
import eu.sim642.adventofcode2017.Day14.PosGrid

object Day13 {

  case class Cart(pos: Pos, direction: Pos, intersections: Int = 0) {
    def tick: Cart = copy(pos = pos + direction)
  }

  val leftDirections = Seq(Pos(1, 0), Pos(0, -1), Pos(-1, 0), Pos(0, 1))

  def tickCarts(grid: Grid[Char], carts: Seq[Cart]): Seq[Cart] = {
    //println(carts)
    carts.map(_.tick).map({ case cart@Cart(pos, direction, intersections) =>
      grid(pos) match {
        case '+' =>
          val direction2 = intersections % 3 match {
            case 0 => leftDirections((leftDirections.indexOf(direction) + 1) % 4) // left
            case 1 => direction // straight
            case 2 => leftDirections((leftDirections.indexOf(direction) + 3) % 4)// right
          }
          cart.copy(direction = direction2, intersections = intersections + 1)
        case '/' if direction.x == 0 => cart.copy(direction = Pos(-direction.y, 0))
        case '\\' if direction.x == 0 => cart.copy(direction = Pos(direction.y, 0))
        case '/' if direction.y == 0 => cart.copy(direction = Pos(0, -direction.x))
        case '\\' if direction.y == 0 => cart.copy(direction = Pos(0, direction.x))
        case _ => cart
      }
    })
  }

  def firstCollisionPos(grid: Grid[Char], carts: Seq[Cart]): Pos = {
    val it = Iterator.iterate(carts)(tickCarts(grid, _))
    val carts2 = it.find(carts => carts.map(_.pos).toSet.size < carts.size).get
    val pos2 = carts2.map(_.pos)
    pos2.diff(pos2.distinct).head
  }

  def lastCartPos(grid: Grid[Char], carts: Seq[Cart]): Pos = {
    def removeCollisions(carts: Seq[Cart]): Seq[Cart] = {
      val poss = carts.map(_.pos)
      val collisionPoss = poss.diff(poss.distinct)
      carts.filterNot(cart => collisionPoss.contains(cart.pos))
    }

    val it = Iterator.iterate(carts)(carts => removeCollisions(tickCarts(grid, carts)))
    it.find(carts => carts.size == 1).get.head.pos
  }

  def parseGrid(input: String): Grid[Char] = input.lines.map(_.toVector).toVector

  def parseCart(cell: Char, pos: Pos): Option[Cart] = {
    val direction: Option[Pos] = cell match {
      case '>' => Some(Pos(1, 0))
      case '<' => Some(Pos(-1, 0))
      case 'v' => Some(Pos(0, 1))
      case '^' => Some(Pos(0, -1))
      case _ => None
    }

    direction.map(Cart(pos, _))
  }

  def parseInput(input: String): (Grid[Char], Seq[Cart]) = {
    val carts: Seq[Cart] = for {
      (row, y) <- parseGrid(input).zipWithIndex
      (cell, x) <- row.zipWithIndex
      pos = Pos(x, y)
      cart <- parseCart(cell, pos)
    } yield cart

    val grid = parseGrid(input.replace('>', '-').replace('<', '-').replace('v', '|').replace('^', '|'))

    (grid, carts)
  }

  def firstCollisionPos(input: String): String = {
    val (grid, carts) = parseInput(input)
    val collisionPos = firstCollisionPos(grid, carts)
    s"${collisionPos.x},${collisionPos.y}"
  }

  def lastCartPos(input: String): String = {
    val (grid, carts) = parseInput(input)
    val collisionPos = lastCartPos(grid, carts)
    s"${collisionPos.x},${collisionPos.y}"
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day13.txt")).mkString.stripLineEnd

  def main(args: Array[String]): Unit = {
    println(firstCollisionPos(input))
    println(lastCartPos(input))

    // 88,50
  }
}
