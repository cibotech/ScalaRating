import com.cibo.evilplot.plot._
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.numeric.{Bounds, Point}
import java.io.File

import com.cibo.evilplot.colors.{Color, HTMLNamedColors}
import com.cibo.evilplot.geometry.Extent
import com.cibo.scalastan.models.SoftKMeans
import com.cibo.scalastan.{RunMethod, ScalaStan}

import scala.collection.mutable



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

  val source = com.cibo.scalastan.data.CsvDataSource.fromFile("nba2015elo.csv")


  val model = new Model {
    for (team <- range(1, n)) {
      skillVector(team) ~ stan.normal(divMu(team), sigma = 1)
    }
    for (game <- range(1, m)) {
      val teamA = teamsA(game)
      val teamB = teamsB(game)
      val skillDelta = skillVector(teamA) - skillVector(teamB)
      teamAWon(game) ~ stan.bernoulli_logit(skillDelta)
    }
  }

  val divMuu: Seq[Double] = Seq(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  //val ys: Seq[Double] = points
  val k = parameter(real())

  val results = model
    .withData(n, 30)
    .withData(divMu, divMuu)
    .withData(m, 1982)
    .withData(teamAWon, teamAWonData)
    .withData(teamsA, teamsAData)
    .withData(teamsB, teamsBData)
    .run(chains = 1, method = RunMethod.Sample(1000, 1000))

  val colors = Color.getGradientSeq(2)

  val teams = Seq("ATL",
    "BKN",
    "BOS",
    "CHA",
    "CHI",
    "CLE",
    "DAL",
    "DEN",
    "DET",
    "GSW",
    "HOU",
    "IND",
    "LAC",
    "LAL",
    "MEM",
    "MIA",
    "MIL",
    "MIN",
    "NOP",
    "NYK",
    "OKC",
    "ORL",
    "PHI",
    "PHX",
    "POR",
    "SAC",
    "SAS",
    "TOR",
    "UTA",
    "WAS")

  val initialElo = Seq(
      1562,
      1470,
      1520,
      1427,
      1570,
      1645,
      1544,
      1443,
      1472,
      1743,
      1617,
      1505,
      1647,
      1339,
      1583,
      1468,
      1459,
      1324,
      1521,
      1318,
      1564,
      1360,
      1333,
      1476,
      1544,
      1440,
      1667,
      1502,
      1543,
      1536
    ).zipWithIndex.map(item => Point(item._2 + 0.5, item._1))

  val carmElo: Seq[Point] = Seq(
      1543,
      1289,
      1573,
      1477,
      1564,
      1732,
      1489,
      1360,
      1452,
      1731,
      1636,
      1465,
      1672,
      1318,
      1609,
      1460,
      1432,
      1345,
      1555,
      1327,
      1690,
      1447,
      1295,
      1485,
      1469,
      1487,
      1685,
      1526,
      1542,
      1598).zipWithIndex.map(item => Point(item._2 + 0.5, item._1))
  val scalaElo = results.mean(skillVector).zipWithIndex.map(item => Point(item._2 + 0.5, item._1 * 100 + 1400))

  val gamesPlayed = teamsAData.zip(teamsBData).foldLeft(mutable.HashMap.empty[(Int, Int), Int])((agg, point) => {
    agg.update(point, agg.getOrElse(point, 0) + 1)
    agg
  })
//  println(gamesPlayed)

  val gamesArray: Array[Array[Double]] = Array.ofDim(30,30)

  gamesPlayed.foreach {case ((a,b), gp) => gamesArray(a-1)(b-1) = gp}
//  gamesArray.foreach(x => {
//    x.foreach(y => print(y + " "))
//    println()
//  })

  val kModel = SoftKMeans(2, gamesArray.map(_.toSeq))
  val kResults = kModel.compile.run(chains = 1, method = RunMethod.Sample(1000, 1000))
  kResults.summary(System.out)

  Overlay(
    LinePlot.series(scalaElo, "Scala ELO", HTMLNamedColors.red),
    LinePlot.series(initialElo, "538 initial ELO", HTMLNamedColors.blue),
    LinePlot.series(carmElo, "538 CARM-ELO", HTMLNamedColors.green)
  ).standard(teams).xGrid(Some(60)).rightLegend().render(Extent(1500, 500)).write(new File("plotELO.png"))

  results.summary(System.out)
  val topFive: Seq[Seq[Point]] = Seq(scalaElo.sortBy(_._2).reverse, initialElo.sortBy(_._2).reverse,
    carmElo.sortBy(_._2).reverse)
  val topFiveLabels: Seq[Seq[String]] = Seq(topFive(0).map(x => teams((x._1 - 0.5).toInt)), topFive(1).map(x => teams((x._1 - 0.5).toInt)),
    topFive(2).map(x => teams((x._1 - 0.5).toInt)))

  Facets(Seq(Seq(BarChart(topFive(0).map(_._2)).xAxis(topFiveLabels(0)).ybounds(1200, 1800)), Seq(BarChart(topFive(1).map(_._2))
    .xAxis(topFiveLabels(1)).ybounds(1200, 1800)))).yAxis(Some(6)).xGrid().yGrid().render(Extent(1500, 1000)).write(new File("plotInOrder.png"))

}


