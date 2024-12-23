package eu.sim642.adventofcodelib.graph

object BronKerbosch {

  // unused
  def maximalCliques[A](neighbors: Map[A, Set[A]]): Iterator[Set[A]] = {

    def bronKerbosch(r: Set[A], p: Set[A], x: Set[A]): Iterator[Set[A]] = {
      if (p.isEmpty) {
        if (x.isEmpty)
          Iterator.single(r)
        else
          Iterator.empty
      }
      else {
        //val u = p.headOption.getOrElse(x.head)
        val u = (p ++ x).maxBy(neighbors(_).size) // pivot on highest degree
        val vs = (p -- neighbors(u)).iterator
        Iterator.unfold((p, x))((p, x) => // foldLeftFlatMap
          vs.nextOption().map(v =>
            (bronKerbosch(r + v, p intersect neighbors(v), x intersect neighbors(v)), (p - v, x + v))
          )
        ).flatten
      }
    }

    bronKerbosch(Set.empty, neighbors.keySet, Set.empty)
  }

  // TODO: would this be slower than direct implementation below?
  /*def maximumClique[A](neighbors: Map[A, Set[A]]): Set[A] =
    maximalCliques(neighbors).maxBy(_.size)*/

  // moved from 2018 day 23
  def maximumClique[A](neighbors: Map[A, Set[A]]): Set[A] = {
    var best: Set[A] = Set.empty

    def bronKerbosch(r: Set[A], p: Set[A], x: Set[A]): Unit = {
      if (p.isEmpty && x.isEmpty) {
        //println(r)
        if (r.size > best.size)
          best = r
      }
      else {
        //val u = p.headOption.getOrElse(x.head)
        val u = (p ++ x).maxBy(neighbors(_).size) // pivot on highest degree
        var p2 = p
        var x2 = x
        for (v <- p -- neighbors(u)) {
          bronKerbosch(r + v, p2 intersect neighbors(v), x2 intersect neighbors(v))
          p2 -= v
          x2 += v
        }
      }
    }

    bronKerbosch(Set.empty, neighbors.keySet, Set.empty)
    best
  }
}
