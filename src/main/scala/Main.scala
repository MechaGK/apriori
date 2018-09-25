import Datasets.Datasets
import Apriori.Apriori

object Main extends App {
  val apriori = new Apriori(Datasets.assignment1)

  println(apriori.run(2))
}