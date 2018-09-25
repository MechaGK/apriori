package Apriori

import Apriori.Types._
import Datasets.Dataset

import scala.collection.SortedSet

class Apriori[A: Ordering](val dataset: Dataset[Char]) {
  def run(threshold: Int): AprioriRun = {
    val items = dataset.items
    val transactions = dataset.transactions

    val counted = transactions.foldLeft(items.map(i => i -> 0).toMap: Map[Char, Int]) { (map, transaction) =>
      map.map { case kv@(key, value) =>
        if (transaction(key)) {
          (key, value + 1)
        } else {
          kv
        }
      }
    }

    val frequentItems = counted.filter { case (_, count) => count >= threshold }.keys.map(ItemSet(_)).toSeq

    val baseStep: AprioriStep[Char] = AprioriStep.fromMap[Char](
      start = frequentItems,
      GenerateCandidatesRun(frequentItems.map(GeneratedItemSet(_, Seq.empty)), frequentItems.map(PrunedItemSet(_, Seq.empty)), frequentItems),
      counted.map { case (k, v) => (ItemSet(k), v) },
      frequentItems
    )

    AprioriRun(
      runs = runStep(Seq(baseStep), threshold),
      threshold = threshold)
  }

  def tryJoin(a: ItemSet[Char], b: ItemSet[Char]): Option[GeneratedItemSet] = {
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

  def getJoins(itemsets: Seq[ItemSet[Char]]): Seq[GeneratedItemSet] = itemsets.toList match {
    case Nil => Seq.empty
    case _ :: Nil => Seq.empty
    case element :: sets => sets.flatMap(tryJoin(_, element)) ++ getJoins(sets)
  }

  def generateCandidates(itemsets: Seq[ItemSet[Char]]): GenerateCandidatesRun[Char] = {
    val allCandidates = getJoins(itemsets)
    val prunedCandidates = allCandidates.map(_.itemSet).map(s => PrunedItemSet(
      itemSet = s,
      missingItemSets = s.subsets(s.size - 1).filterNot(itemsets.contains(_)).toSeq))
    val finalCandidates = allCandidates.map(_.itemSet).diff(prunedCandidates.filter(_.missingItemSets.nonEmpty).map(_.itemSet))
    GenerateCandidatesRun(
      allCandidates = allCandidates,
      prunedCandidates = prunedCandidates,
      finalCandidates = finalCandidates
    )
  }

  def runStep(itemset: Seq[AprioriStep[Char]], threshold: Int): Seq[AprioriStep[Char]] = {
    if (itemset.last.frequentItemSets.isEmpty) {
      itemset
    } else {
      runStep(itemset :+ apriori(itemset.last.frequentItemSets, threshold), threshold)
    }
  }

  def apriori(itemset: Seq[SortedSet[Char]], threshold: Int): AprioriStep[Char] = {
    val candidates = generateCandidates(itemset)

    val counted = dataset.transactions.foldLeft(
      candidates.finalCandidates.map(s => s -> 0).toMap: Map[SortedSet[Char], Int]) { (map, transaction) =>
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
      start = itemset,
      generateCandidatesRun = candidates,
      itemSetMap = counted,
      frequentItemSets = filtered.keys.toSeq
    )
  }
}