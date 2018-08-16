import com.cibo.evilplot.colors.{
  ContinuousColoring,
  GradientMode,
  HTMLNamedColors
}
import com.cibo.evilplot.displayPlot
import com.cibo.evilplot.geometry.{Disc, Drawable, Extent, Text}
import com.cibo.evilplot.numeric.Point
import com.cibo.evilplot.plot.{
  Bar,
  BarChart,
  BoxPlot,
  Overlay,
  Plot,
  ScatterPlot
}
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.plot.components.{Marker, Position}
import com.cibo.evilplot.plot.renderers.{BarRenderer, PointRenderer}

object dataVisualization {
  val teams = Seq(
    team("ATL"),
    team("BKN"),
    team("BOS"),
    team("CHA"),
    team("CHI"),
    team("CLE"),
    team("DAL"),
    team("DEN"),
    team("DET"),
    team("GSW"),
    team("HOU"),
    team("IND"),
    team("LAC"),
    team("LAL"),
    team("MEM"),
    team("MIA"),
    team("MIL"),
    team("MIN"),
    team("NOP"),
    team("NYK"),
    team("OKC"),
    team("ORL"),
    team("PHI"),
    team("PHX"),
    team("POR"),
    team("SAC"),
    team("SAS"),
    team("TOR"),
    team("UTA"),
    team("WAS")
  )

  def initialBox(results: Seq[Seq[Double]]): Unit = {
    displayPlot(
      BoxPlot(results)
        .standard(teams.map(_.name))
        .render(Extent(1500, 500)))
  }

  def sortedColoredPlot(results: Seq[Double]): Unit = {
    val colors =
      ContinuousColoring.gradient(Seq(HTMLNamedColors.dodgerBlue,
                                      HTMLNamedColors.crimson,
                                      HTMLNamedColors.green),
                                  Some(-0.5),
                                  Some(1.8),
                                  GradientMode.Linear)
    val sortedPairs = results.zip(teams).sortBy(_._1)
    val depthPointRender =
      PointRenderer.depthColor(sortedPairs.map(_._1), Some(colors), Some(8.0))

    val scatter = ScatterPlot(sortedPairs.map(_._1).zipWithIndex.map {
      case (y, x) => Point(x - 0.5, y)
    }, Some(depthPointRender))
      .standard(sortedPairs.map(_._2.name))
      .rightLegend()
      .render(Extent(1500, 500))

    displayPlot(scatter)
  }

  def outputVisualization(scalaElo: Seq[Point]): Unit = {

    val initialEloValues: Seq[Double] = teams.map(team => team.getStats._2)
    val initialEloStats = BasicStats.stats(initialEloValues)
    val initialElo = initialEloValues.zipWithIndex.map(item =>
      Point(item._2 + 0.5, (item._1 - initialEloStats._1) / initialEloStats._2))

    val recordEloValues: Seq[Double] = teams.map(team => team.getStats._1)
    val recordEloStats = BasicStats.stats(recordEloValues)
    val recordElo = recordEloValues.zipWithIndex.map(item =>
      Point(item._2 + 0.5, (item._1 - recordEloStats._1) / recordEloStats._2))

    val allDifferences = BasicStats.averageDist(teams.indices.map(i =>
      Seq(scalaElo(i).y, initialElo(i).y, recordElo(i).y)))

    val discRenderer = new BarRenderer {
      def render(plot: Plot, extent: Extent, category: Bar): Drawable = {
        Disc(extent.height)
          .transX(extent.width / 2 - extent.height)
          .filled(HTMLNamedColors.grey)
      }
    }

    val lines = Overlay(
      ScatterPlot.series(initialElo,
                         "538 Initial ELO",
                         HTMLNamedColors.blue,
                         pointSize = Some(6)),
      ScatterPlot.series(recordElo,
                         "Win/Loss Record",
                         HTMLNamedColors.green,
                         pointSize = Some(6)),
      ScatterPlot.series(scalaElo,
                         "Scala ELO",
                         HTMLNamedColors.red,
                         pointSize = Some(6))
    ).topPlot(BarChart.custom(allDifferences.map(i => Bar(i)),
                               barRenderer = Some(discRenderer)))
      .standard(teams.map(_.name))
      .xGrid(Some(30))
      .rightLegend()
      .hline(0.0)
      .render(Extent(1500, 600))

    displayPlot(lines)

  }

  def stackedComparison(scalaElo: Seq[Point]): Unit = {

    val initialEloValues: Seq[Double] = teams.map(team => team.getStats._2)
    val initialEloStats = BasicStats.stats(initialEloValues)
    val initialElo = initialEloValues.zipWithIndex.map(item =>
      Point(item._2 + 0.5, (item._1 - initialEloStats._1) / initialEloStats._2))

    val recordEloValues: Seq[Double] = teams.map(team => team.getStats._1)
    val recordEloStats = BasicStats.stats(recordEloValues)
    val recordElo = recordEloValues.zipWithIndex.map(item =>
      Point(item._2 + 0.5, (item._1 - recordEloStats._1) / recordEloStats._2))

    val stackedBars: Seq[Seq[Double]] = teams.indices.map(i =>
      Seq(scalaElo(i).y, initialElo(i).y, recordElo(i).y))
    val barChart = BarChart
      .stacked(stackedBars,
               Seq(HTMLNamedColors.red,
                   HTMLNamedColors.blue,
                   HTMLNamedColors.green),
               Seq("Scala Elo", "538 Elo", "Win/Loss Record"))
      .component(
        Marker(Position.Overlay,
               _ => Text("< Champs!", 20) rotated (-90),
               Extent(20, 20),
               x = 9.5,
               y = 6))
      .standard(teams.map(_.name))
      .rightLegend()
      .hline(0.0)
      .render(Extent(1500, 500))
    displayPlot(barChart)
  }

  def visualizePriors(priors: Seq[Point]): Unit = {
    val scatter = ScatterPlot(priors)
      .standard(teams.map(_.name))
      .ybounds(-1, 2)
      .render(Extent(1500, 500))
    displayPlot(scatter)
  }

}
