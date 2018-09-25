package Apriori

import scala.collection.SortedSet

object Types {
  type ItemSet[Ordering] = SortedSet[Ordering]

  object ItemSet {
    def apply[A: Ordering](item: A): ItemSet[A] = SortedSet(item)
  }

  case class GeneratedItemSet[A: Ordering](
                                            itemSet: ItemSet[A],
                                            parents: Seq[ItemSet[A]]
                                          )

  object GeneratedItemSet {
    def fromItemSets[A: Ordering](a: ItemSet[A], b: ItemSet[A]): GeneratedItemSet[A] = {
      GeneratedItemSet(
        itemSet = a ++ b,
        parents = Seq(a, b)
      )
    }
  }


  case class PrunedItemSet[A: Ordering](
                                         itemSet: ItemSet[A],
                                         missingItemSets: Seq[ItemSet[A]]
                                       )

  case class GenerateCandidatesRun[A: Ordering](
                                                 allCandidates: Seq[GeneratedItemSet[A]],
                                                 prunedCandidates: Seq[PrunedItemSet[A]],
                                                 finalCandidates: Seq[ItemSet[A]]
                                               )

  case class ItemSetFrequency[A: Ordering](
                                            itemSet: ItemSet[A],
                                            frequency: Int
                                          )

  case class AprioriStep[A: Ordering](
                                       generateCandidatesRun: GenerateCandidatesRun[A],
                                       itemSetsFrequency: Seq[ItemSetFrequency[A]],
                                       frequentItemSets: Seq[ItemSet[A]]
                                     )

  object AprioriStep {
    def fromMap[A: Ordering](generateCandidatesRun: GenerateCandidatesRun[A], itemSetMap: Map[ItemSet[A], Int], frequentItemSets: Seq[ItemSet[A]]): AprioriStep[A] = {
      AprioriStep(
        generateCandidatesRun,
        itemSetMap.map { case (k, v) =>
          ItemSetFrequency[A](
            itemSet = k,
            frequency = v
          )
        }.toSeq,
        frequentItemSets
      )
    }
  }

}
