package eu.sim642.adventofcodelib

import eu.sim642.adventofcodelib.UnionFind.Node

import scala.annotation.tailrec

class UnionFind[A](val nodes: Map[A, Node[A]]) {

  def this(xs: Seq[A]) = {
    this(xs.map(x => x -> Node(x, 1)).toMap)
  }

  @tailrec
  final def findReprNode(x: A): Node[A] = {
    // TODO: path compression
    val node = nodes(x)
    if (x == node.parent)
      node
    else
      findReprNode(node.parent)
  }

  def findRepr(x: A): A = findReprNode(x).parent

  def sameRepr(x: A, y: A): Boolean =
    findRepr(x) == findRepr(y)

  def unioned(x: A, y: A): UnionFind[A] = {
    val xNode = findReprNode(x)
    val yNode = findReprNode(y)
    val xRepr = xNode.parent
    val yRepr = yNode.parent
    if (xRepr == yRepr) // sameRepr inlined
      this
    else if (xNode.size >= yNode.size)
      new UnionFind(nodes + (xRepr -> xNode.copy(size = xNode.size + yNode.size)) + (yRepr -> yNode.copy(parent = xRepr)))
    else
      new UnionFind(nodes + (yRepr -> yNode.copy(size = xNode.size + yNode.size)) + (xRepr -> xNode.copy(parent = yRepr)))
  }

  def components: Iterable[Iterable[A]] =
    nodes.keys.groupBy(findRepr).values

  def rootNodes: Iterable[Node[A]] =
    nodes.view.filter((x, node) => x == node.parent).values

  override def toString: String = nodes.toString()
}

object UnionFind {
  case class Node[A](parent: A, size: Int)
}
