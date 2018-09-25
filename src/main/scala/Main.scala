import Datasets.Datasets
import Apriori._

object Main extends App {
  val apriori: Apriori[Char] = new Apriori(Datasets.assignment1)
  val runs = apriori.run(2)

  println(html.apriorirun.render(runs))
}
