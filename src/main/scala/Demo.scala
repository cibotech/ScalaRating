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

import com.cibo.evilplot.numeric.Point

import com.cibo.scalastan.{RunMethod, ScalaStan}

object Demo extends App with ScalaStan {

  val n = data(int(lower = 0))
  val divMu = data(real()(n))
  val m = data(int(lower = 0))
  val teamAWon = data(int(lower = 0, upper = 1)(m))
  val teamsA = data(int(lower = 0)(m))
  val teamsB = data(int(lower = 0)(m))

  val arrays = DataParser.runCSVParser()

  val teamsAData = arrays(1)
  val teamsBData = arrays(2)
  val teamAWonData = arrays(0)

  val skillVector = parameter(real()(n))

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

  val scalaStats = BasicStats.stats(results.mean(skillVector))

  val scalaRating = results
    .mean(skillVector)
    .zipWithIndex
    .map(item =>
      Point(item._2 + 0.5, (item._1 - scalaStats._1) / scalaStats._2))

  results.summary(System.out)

  val scalaDoubles: Seq[Double] = results
    .mean(skillVector)

  val boxStats: Seq[Seq[Double]] =
    results.samples(skillVector).flatten.transpose

  DataVisualization.visualizePriors(
    divMuInitial.zipWithIndex.map(pair => Point(pair._2 + 0.5, pair._1)))
  DataVisualization.initialBox(boxStats)
  DataVisualization.sortedColoredPlot(scalaDoubles)
  DataVisualization.stackedComparison(scalaRating)
  DataVisualization.outputVisualization(scalaRating)
}
