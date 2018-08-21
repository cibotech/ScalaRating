/*
 * Copyright (c) 2018, CiBO Technologies, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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

object DataVisualization {
  val teams = Seq(
    Team("ATL"),
    Team("BKN"),
    Team("BOS"),
    Team("CHA"),
    Team("CHI"),
    Team("CLE"),
    Team("DAL"),
    Team("DEN"),
    Team("DET"),
    Team("GSW"),
    Team("HOU"),
    Team("IND"),
    Team("LAC"),
    Team("LAL"),
    Team("MEM"),
    Team("MIA"),
    Team("MIL"),
    Team("MIN"),
    Team("NOP"),
    Team("NYK"),
    Team("OKC"),
    Team("ORL"),
    Team("PHI"),
    Team("PHX"),
    Team("POR"),
    Team("SAC"),
    Team("SAS"),
    Team("TOR"),
    Team("UTA"),
    Team("WAS")
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

  def outputVisualization(scalaRating: Seq[Point]): Unit = {

    val initialEloValues: Seq[Double] = teams.map(team => team.getStats._2)
    val initialEloStats = BasicStats.stats(initialEloValues)
    val initialElo = initialEloValues.zipWithIndex.map(item =>
      Point(item._2 + 0.5, (item._1 - initialEloStats._1) / initialEloStats._2))

    val recordEloValues: Seq[Double] = teams.map(team => team.getStats._1)
    val recordEloStats = BasicStats.stats(recordEloValues)
    val recordElo = recordEloValues.zipWithIndex.map(item =>
      Point(item._2 + 0.5, (item._1 - recordEloStats._1) / recordEloStats._2))

    val allDifferences = BasicStats.averageDist(teams.indices.map(i =>
      Seq(scalaRating(i).y, initialElo(i).y, recordElo(i).y)))

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
      ScatterPlot.series(scalaRating,
                         "ScalaStan Rating",
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

  def stackedComparison(scalaRating: Seq[Point]): Unit = {

    val initialEloValues: Seq[Double] = teams.map(team => team.getStats._2)
    val initialEloStats = BasicStats.stats(initialEloValues)
    val initialElo = initialEloValues.zipWithIndex.map(item =>
      Point(item._2 + 0.5, (item._1 - initialEloStats._1) / initialEloStats._2))

    val recordEloValues: Seq[Double] = teams.map(team => team.getStats._1)
    val recordEloStats = BasicStats.stats(recordEloValues)
    val recordElo = recordEloValues.zipWithIndex.map(item =>
      Point(item._2 + 0.5, (item._1 - recordEloStats._1) / recordEloStats._2))

    val stackedBars: Seq[Seq[Double]] = teams.indices.map(i =>
      Seq(scalaRating(i).y, initialElo(i).y, recordElo(i).y))
    val barChart = BarChart
      .stacked(stackedBars,
               Seq(HTMLNamedColors.red,
                   HTMLNamedColors.blue,
                   HTMLNamedColors.green),
               Seq("ScalaStan Rating", "538 Elo", "Win/Loss Record"))
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
