package Apriori

import Apriori.Types._
import Datasets.Dataset

import scala.collection.SortedSet

class Apriori[A: Ordering](val dataset: Dataset[A]) {
  def run(threshold: Int): Seq[AprioriStep[A]] = {
    val items = dataset.items
    val transactions = dataset.transactions

    val counted = transactions.foldLeft(items.map(i => i -> 0).toMap: Map[A, Int]) { (map, transaction) =>
      map.map { case kv@(key, value) =>
        if (transaction(key)) {
          (key, value + 1)
        } else {
          kv
        }
      }
    }

    val frequentItems = counted.filter { case (_, count) => count >= threshold }.keys.map(ItemSet(_)).toSeq

    val baseStep: AprioriStep[A] = AprioriStep.fromMap[A](
      GenerateCandidatesRun(Seq.empty, Seq.empty, Seq.empty),
      counted.map { case (k, v) => (ItemSet(k), v) },
      frequentItems
    )

    runStep(Seq(baseStep), threshold)
  }

  def tryJoin(a: ItemSet[A], b: ItemSet[A]): Option[GeneratedItemSet[A]] = {
    if (a == b) {
      None
    } else {
      val af = a - a.last
      val bf = b - b.last
      if (af == bf) {
        Some(GeneratedItemSet.fromItemSets(a, b))
      } else {
        None
      }
    }
  }

  def getJoins(itemsets: Seq[ItemSet[A]]): Seq[GeneratedItemSet[A]] = itemsets.toList match {
    case Nil => Seq.empty
    case _ :: Nil => Seq.empty
    case element :: sets => sets.flatMap(tryJoin(_, element)) ++ getJoins(sets)
  }

  def generateCandidates(itemsets: Seq[ItemSet[A]]): GenerateCandidatesRun[A] = {
    val allCandidates = getJoins(itemsets)
    val prunedCandidates = allCandidates.map(_.itemSet).map(s => PrunedItemSet(
      itemSet = s,
      missingItemSets = s.subsets(s.size - 1).filter(itemsets.contains(_)).toSeq)).filter(_.missingItemSets.isEmpty)
    val finalCandidates = allCandidates.map(_.itemSet).diff(prunedCandidates.map(_.itemSet))

    GenerateCandidatesRun(
      allCandidates = allCandidates,
      prunedCandidates = prunedCandidates,
      finalCandidates = finalCandidates
    )
  }

  def runStep(itemset: Seq[AprioriStep[A]], threshold: Int): Seq[AprioriStep[A]] = {
    if (itemset.last.frequentItemSets.isEmpty) {
      itemset
    } else {
      runStep(itemset :+ apriori(itemset.last.frequentItemSets, threshold), threshold)
    }
  }

  def apriori(itemset: Seq[SortedSet[A]], threshold: Int): AprioriStep[A] = {
    val candidates = generateCandidates(itemset)

    val counted = dataset.transactions.foldLeft(
      candidates.finalCandidates.map(s => s -> 0).toMap: Map[SortedSet[A], Int]) { (map, transaction) =>
      map.map { case kv@(key, value) =>
        if (key subsetOf transaction) {
          (key, value + 1)
        } else {
          kv
        }
      }
    }
    val filtered = counted.filter { case (_, count) => count >= threshold }

    AprioriStep.fromMap(
      generateCandidatesRun = candidates,
      itemSetMap = counted,
      frequentItemSets = filtered.keys.toSeq
    )
  }
}