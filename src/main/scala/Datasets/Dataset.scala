package Datasets

import Apriori.Types.ItemSet

case class Dataset[A: Ordering](
                                 items: ItemSet[A],
                                 transactions: Seq[ItemSet[A]]
                               )
