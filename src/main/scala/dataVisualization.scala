import java.io.File

import com.cibo.evilplot.colors.{Color, ContinuousColoring, GradientMode, HTMLNamedColors}
import com.cibo.evilplot.geometry.{Disc, Drawable, Extent, Text}
import com.cibo.evilplot.numeric.Point
import com.cibo.evilplot.plot.{Bar, BarChart, BoxPlot, Facets, Histogram, LinePlot, Overlay, Plot, ScatterPlot}
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.plot.components.{Marker, Position}
import com.cibo.evilplot.plot.renderers.{BarRenderer, PointRenderer}
import com.cibo.scalastan.StanResults

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
    BoxPlot(results)
      .standard(teams.map(_.name))
      .render(Extent(1500, 500))
      .write(new File("boxPlot.png"))
  }

  def sortedColoredPlot(results: Seq[Double]): Unit = {
    val colors =
      ContinuousColoring.gradient(Seq(HTMLNamedColors.dodgerBlue, HTMLNamedColors.crimson, HTMLNamedColors.dodgerBlue),
        Some(-1.0), Some(2.0), GradientMode.Linear)
    val sortedPairs = results.zip(teams).sortBy(_._1)
    val depthPointRender =
      PointRenderer.depthColor(sortedPairs.map(_._1), Some(colors))
    ScatterPlot(sortedPairs.map(_._1).zipWithIndex.map {
      case (y, x) => Point(x - 0.5, y)
    }, Some(depthPointRender))
      .standard(sortedPairs.map(_._2.name))
      .rightLegend()
      .render(Extent(1500, 500))
      .write(new File("sortedMeanPlot.png"))
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
        Disc(extent.height) translate (extent.width / 2 - extent.height, 0) filled (HTMLNamedColors.grey)
      }

    }

    Overlay(
      LinePlot.series(scalaElo, "Scala ELO", HTMLNamedColors.red),
      LinePlot.series(initialElo, "538 Initial ELO", HTMLNamedColors.blue),
      LinePlot.series(recordElo, "Win/Loss Record", HTMLNamedColors.green)
    ).topPlot(
        BarChart.custom(allDifferences.map(x => Bar(x)), Some(discRenderer)))
      .standard(teams.map(_.name))
      .xGrid(Some(60))
      .rightLegend()
      .hline(0.0)
      .render(Extent(1500, 600))
      .write(new File("plotELO.png"))

  }

  def topFive = {
//        Facets(
//          Seq(
//            Seq(
//              BarChart(topFive(0).map(_._2), color = Some(HTMLNamedColors.red))
//                .title("Scala ELO")
//                .standard(topFiveLabels(0))
//                .ybounds(0, 3),
//              BarChart(topFive(2).map(_._2), color = Some(HTMLNamedColors.blue))
//                .title("538 Initial ELO")
//                .standard(topFiveLabels(2))
//                .ybounds(0, 3)
//            ),
//            Seq(
//              BarChart(topFive(3).map(_._2), color = Some(HTMLNamedColors.green))
//                .title("Win/Loss Record")
//                .standard(topFiveLabels(3))
//                .ybounds(0, 3),
//              BarChart(topFive(1).map(_._2), color = Some(HTMLNamedColors.orange))
//                .title("CARM-ELO")
//                .standard(topFiveLabels(1))
//                .ybounds(0, 3)
//            )
//          ))
//          .render(Extent(1500, 1000))
//          .write(new File("plotInOrder.png"))

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

    BarChart
      .stacked(teams.indices.map(i =>
        Seq(scalaElo(i).y, initialElo(i).y, recordElo(i).y)),
        Seq(HTMLNamedColors.red,
          HTMLNamedColors.blue,
          HTMLNamedColors.green))
      .component(
        Marker(Position.Overlay,
          _ => Text("<- Champs!", 20) rotated (-90),
          Extent(20, 20),
          x = 9.5,
          y = 6))
      .standard(teams.map(_.name))
      .hline(0.0)
      .render(Extent(1500, 500))
      .write(new File("stackedPlot.png"))
  }

}
