package eu.sim642.adventofcodelib.graph

object NaiveTSP {

  // TODO: move to generic TSP object or generalize from Map
  type DistanceMatrix[A] = Map[A, Map[A, Int]]

  // TODO: extract also exact path in future use?

  private def totalPathLength[A](distanceMatrix: DistanceMatrix[A], path: Seq[A]): Int = {
    path.zip(path.tail)
      .map({ case (from, to) => distanceMatrix(from)(to) })
      .sum
  }

  // moved & modified from 2016 Day 24 via 2015 Day 9
  def pathLength[A](distanceMatrix: DistanceMatrix[A])(implicit lengthOrdering: Ordering[Int]): Int = {
    distanceMatrix.keySet.toVector
      .permutations
      .map({ path =>
        totalPathLength(distanceMatrix, path)
      }).min(lengthOrdering)
  }

  // moved from 2016 Day 24
  def startPathLength[A](distanceMatrix: DistanceMatrix[A], start: A)(implicit lengthOrdering: Ordering[Int]): Int = {
    (distanceMatrix.keySet - start).toVector
      .permutations
      .map({ path =>
        distanceMatrix(start)(path.head) +
          totalPathLength(distanceMatrix, path)
      }).min(lengthOrdering)
  }

  // moved from 2016 Day 24
  def cycleLength[A](distanceMatrix: DistanceMatrix[A], start: A)(implicit lengthOrdering: Ordering[Int]): Int = {
    (distanceMatrix.keySet - start).toVector
      .permutations
      .map({ path =>
        distanceMatrix(start)(path.head) +
          totalPathLength(distanceMatrix, path) +
          distanceMatrix(path.last)(start)
      }).min(lengthOrdering)
  }

  def cycleLength[A](distanceMatrix: DistanceMatrix[A])(implicit lengthOrdering: Ordering[Int]): Int = {
    cycleLength(distanceMatrix, distanceMatrix.keys.head)(lengthOrdering)
  }
}
