package Apriori

import scala.collection.SortedSet

object Types {
  type ItemSet[Ordering] = SortedSet[Ordering]

  object ItemSet {
    def apply[A: Ordering](item: A): ItemSet[A] = SortedSet(item)

    implicit def compareInt(a: ItemSet[Char], b: ItemSet[Char]): Int = (a.toSeq zip b.toSeq).map { case (i1, i2) => i1 compare i2 }.find(_ != 0).getOrElse(a.size compare b.size)

    implicit def compare(a: ItemSet[Char], b: ItemSet[Char]): Boolean = compareInt(a, b) < 0
  }

  /*
    implicit object ItemSetOrdering extends Ordering[ItemSet[Char]] {

    }
  */
  case class GeneratedItemSet(
                               itemSet: ItemSet[Char],
                               parents: Seq[ItemSet[Char]]
                             ) extends Ordered[GeneratedItemSet] {
    def compare(that: GeneratedItemSet): Int = ItemSet.compareInt(this.itemSet, that.itemSet)
  }

  object GeneratedItemSet {
    def fromItemSets(a: ItemSet[Char], b: ItemSet[Char]): GeneratedItemSet = {
      GeneratedItemSet(
        itemSet = a ++ b,
        parents = Seq(a, b)
      )
    }
  }


  case class PrunedItemSet(
                            itemSet: ItemSet[Char],
                            missingItemSets: Seq[ItemSet[Char]]
                          ) extends Ordered[PrunedItemSet] {
    val missingItems: SortedSet[Char] = missingItemSets.fold(SortedSet.empty[Char])((a, b) => a ++ b)

    def compare(that: PrunedItemSet): Int = ItemSet.compareInt(this.itemSet, that.itemSet)
  }

  case class GenerateCandidatesRun[A: Ordering](
                                                 allCandidates: Seq[GeneratedItemSet],
                                                 prunedCandidates: Seq[PrunedItemSet],
                                                 finalCandidates: Seq[ItemSet[A]]
                                               )

  case class ItemSetFrequency(
                               itemSet: ItemSet[Char],
                               frequency: Int
                             ) extends Ordered[ItemSetFrequency] {
    def compare(that: ItemSetFrequency): Int = ItemSet.compareInt(this.itemSet, that.itemSet)
  }

  case class AprioriStep[A: Ordering](
                                       start: Seq[ItemSet[A]],
                                       generateCandidatesRun: GenerateCandidatesRun[A],
                                       itemSetsFrequency: Seq[ItemSetFrequency],
                                       frequentItemSets: Seq[ItemSet[A]]
                                     )

  object AprioriStep {
    def fromMap[A: Ordering](start: Seq[ItemSet[A]], generateCandidatesRun: GenerateCandidatesRun[A], itemSetMap: Map[ItemSet[Char], Int], frequentItemSets: Seq[ItemSet[A]]): AprioriStep[A] = {
      AprioriStep(
        start,
        generateCandidatesRun,
        itemSetMap.map { case (k, v) =>
          ItemSetFrequency(
            itemSet = k,
            frequency = v
          )
        }.toSeq,
        frequentItemSets
      )
    }
  }

  case class AprioriRun(
                         runs: Seq[AprioriStep[Char]],
                         threshold: Int,
                       )

}
