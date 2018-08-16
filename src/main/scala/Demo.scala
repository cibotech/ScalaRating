import com.cibo.evilplot.numeric.Point

import com.cibo.scalastan.{RunMethod, ScalaStan}

object demo extends App with ScalaStan {

  val n = data(int(lower = 0))
  val divMu = data(real()(n))
  val m = data(int(lower = 0))
  val teamAWon = data(int(lower = 0, upper = 1)(m))
  val teamsA = data(int(lower = 0)(m))
  val teamsB = data(int(lower = 0)(m))

  val arrays = dataParser.runCSVParser()

  val teamsAData = arrays(1)
  val teamsBData = arrays(2)
  val teamAWonData = arrays(0)

  val skillVector = parameter(real()(n))

  val model = new Model {
    for (team <- range(1, n)) {
      skillVector(team) ~ stan.normal(divMu(team), sigma = 0.5)
    }
    for (game <- range(1, m)) {
      val teamA = teamsA(game)
      val teamB = teamsB(game)
      val skillDelta = skillVector(teamA) - skillVector(teamB)
      teamAWon(game) ~ stan.bernoulli_logit(skillDelta)
    }
  }

  val divMuInitial: Seq[Double] = Seq(1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0,
    1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1)
  val k = parameter(real())

  val results = model
    .withData(n, 30)
    .withData(divMu, divMuInitial)
    .withData(m, 2460)
    .withData(teamAWon, teamAWonData)
    .withData(teamsA, teamsAData)
    .withData(teamsB, teamsBData)
    .run(chains = 4, method = RunMethod.Sample(1000, 1000))

  val scalaEloStats = BasicStats.stats(results.mean(skillVector))

  val scalaElo = results
    .mean(skillVector)
    .zipWithIndex
    .map(item =>
      Point(item._2 + 0.5, (item._1 - scalaEloStats._1) / scalaEloStats._2))

  results.summary(System.out)

  val scalaEloDoubles: Seq[Double] = results
    .mean(skillVector)

  val boxStats: Seq[Seq[Double]] =
    results.samples(skillVector).flatten.transpose

  dataVisualization.visualizePriors(
    divMuInitial.zipWithIndex.map(pair => Point(pair._2 + 0.5, pair._1)))
  dataVisualization.initialBox(boxStats)
  dataVisualization.sortedColoredPlot(scalaEloDoubles)
  dataVisualization.stackedComparison(scalaElo)
  dataVisualization.outputVisualization(scalaElo)
}
