package Datasets

import scala.collection.SortedSet

object Datasets {
  val assignment1: Dataset[Char] = Dataset(
    SortedSet('A', 'B', 'C', 'D', 'E', 'F'),
    Seq(
      SortedSet('A', 'B', 'E'),
      SortedSet('B', 'D'),
      SortedSet('C', 'D', 'F'),
      SortedSet('A', 'B', 'D'),
      SortedSet('A', 'C', 'E'),
      SortedSet('B', 'C', 'E', 'F'),
      SortedSet('A', 'C', 'E'),
      SortedSet('A', 'B', 'C', 'E'),
      SortedSet('A', 'B', 'C', 'D', 'F'),
      SortedSet('B', 'C', 'D', 'E'),
    ))
}
