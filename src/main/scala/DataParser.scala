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

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object DataParser {

  def runCSVParser(): Seq[ArrayBuffer[Int]] = {
    val teamsA = ArrayBuffer[Int]()
    val teamsB = ArrayBuffer[Int]()
    val teamsAwon = ArrayBuffer[Int]()

    val bufferedSource = Source.fromFile("nba20172018.csv")

    bufferedSource.getLines().drop(1).foreach { line =>
      val row = line.split(",").map(_.trim)
      val (teamA, teamB) = (Team(row(2).split(" vs. | @ ").head),
                            Team(row(2).split(" vs. | @ ").last))
      teamsA += teamA.getID
      teamsB += teamB.getID
      if (row(3).equals("W")) teamsAwon += 1
      else teamsAwon += 0
    }

    println(teamsA.length)
    println(teamsB.length)
    println(teamsAwon.length)
    bufferedSource.close
    Seq(teamsAwon, teamsA, teamsB)

  }

}
