package eu.sim642.adventofcodelib.graph

object BronKerbosch {

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
