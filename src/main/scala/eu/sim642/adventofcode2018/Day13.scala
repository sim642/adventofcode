package eu.sim642.adventofcode2018

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.pos.Pos

object Day13 {

  implicit class DirectionPos(pos: Pos) {
    def left: Pos = Pos(pos.y, -pos.x)
    def right: Pos = Pos(-pos.y, pos.x)

    // Reflections from: https://www.reddit.com/r/adventofcode/comments/a5qd71/2018_day_13_solutions/eboleqg
    def reflectMajor: Pos = Pos(pos.y, pos.x) // \
    def reflectMinor: Pos = Pos(-pos.y, -pos.x) // /
  }

  case class Cart(pos: Pos, direction: Pos, intersections: Int = 0) {
    private def tickMove: Cart = copy(pos = pos + direction)

    private def tickTurn(grid: Grid[Char]): Cart = {
      grid(pos) match {
        case '+' =>
          copy(direction = intersections % 3 match {
            case 0 => direction.left // left
            case 1 => direction // straight
            case 2 => direction.right // right
          }, intersections = intersections + 1)
        case '/' => copy(direction = direction.reflectMinor)
        case '\\' => copy(direction = direction.reflectMajor)
        case _ => this
      }
    }

    def tick(grid: Grid[Char]): Cart = tickMove.tickTurn(grid)
  }

  def tickCarts(grid: Grid[Char], carts: Seq[Cart]): Seq[Cart] = {
    carts.map(_.tick(grid))
  }

  def firstCollisionPos(grid: Grid[Char], carts: Seq[Cart]): Pos = {
    val it = Iterator.iterate(carts)(tickCarts(grid, _))
    val carts2 = it.find(carts => carts.map(_.pos).toSet.size < carts.size).get
    val pos2 = carts2.map(_.pos)
    pos2.diff(pos2.distinct).head
  }

  def tickCartsCollide(grid: Grid[Char], carts: Seq[Cart]): Seq[Cart] = {
    def helper(init: List[Cart], tail: List[Cart]): List[Cart] = tail match {
      case Nil => init
      case cart :: tl =>
        val newCart = cart.tick(grid)
        val init2 = init.filterNot(_.pos == newCart.pos)
        val tl2 = tl.filterNot(_.pos == newCart.pos)
        if (init2.size < init.size || tl2.size < tl.size)
          helper(init2, tl2)
        else
          helper(newCart :: init, tl)
      }

    val sortedCarts = carts.sortWith((c1, c2) => c1.pos.y < c2.pos.y || (c1.pos.y == c2.pos.y && c1.pos.x < c2.pos.x))
    helper(Nil, sortedCarts.toList)
  }

  def lastCartPos(grid: Grid[Char], carts: Seq[Cart]): Pos = {
    val it = Iterator.iterate(carts)(tickCartsCollide(grid, _))
    it.find(_.size == 1).get.head.pos
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

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
    val carts: Seq[Cart] = (for {
      (row, y) <- parseGrid(input).view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      pos = Pos(x, y)
      cart <- parseCart(cell, pos)
    } yield cart).toSeq

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

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day13.txt")).mkString.stripLineEnd

  def main(args: Array[String]): Unit = {
    println(firstCollisionPos(input))
    println(lastCartPos(input))

    // 88,50
    // 110,78
  }
}
