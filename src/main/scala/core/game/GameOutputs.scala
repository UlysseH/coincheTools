package core.game

object GameOutputs {
  case class ForOptimizeAnalysis(
      handMap: (List[String], List[String], List[String], List[String]),
      playedCards: List[String],
      // (step, cards, values)
      vectors: List[(Int, String, List[Int])]
  ) {
    def forWrite = Map(
      "cards1" -> handMap._1.mkString(","),
      "cards2" -> handMap._2.mkString(","),
      "cards3" -> handMap._3.mkString(","),
      "cards4" -> handMap._4.mkString(",")
    )
  }
}
