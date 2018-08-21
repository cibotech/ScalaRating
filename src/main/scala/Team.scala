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

case class Team(name: String) {
  val getID: Int = {
    name match {
      case "ATL" => 1
      case "BKN" => 2
      case "BOS" => 3
      case "CHA" => 4
      case "CHI" => 5
      case "CLE" => 6
      case "DAL" => 7
      case "DEN" => 8
      case "DET" => 9
      case "GSW" => 10
      case "HOU" => 11
      case "IND" => 12
      case "LAC" => 13
      case "LAL" => 14
      case "MEM" => 15
      case "MIA" => 16
      case "MIL" => 17
      case "MIN" => 18
      case "NOP" => 19
      case "NYK" => 20
      case "OKC" => 21
      case "ORL" => 22
      case "PHI" => 23
      case "PHX" => 24
      case "POR" => 25
      case "SAC" => 26
      case "SAS" => 27
      case "TOR" => 28
      case "UTA" => 29
      case "WAS" => 30
    }
  }
  val getStats: (Double, Double) = {
    Vector[(Double, Double)](
      (0.293, 1338),
      (0.341, 1399),
      (0.671, 1585),
      (0.439, 1466),
      (0.329, 1333),
      (0.610, 1570),
      (0.293, 1398),
      (0.561, 1578),
      (0.476, 1470),
      (0.707, 1596),
      (0.793, 1774),
      (0.585, 1589),
      (0.512, 1549),
      (0.427, 1467),
      (0.268, 1324),
      (0.537, 1505),
      (0.537, 1516),
      (0.573, 1556),
      (0.585, 1570),
      (0.354, 1361),
      (0.585, 1641),
      (0.305, 1329),
      (0.634, 1656),
      (0.256, 1231),
      (0.598, 1616),
      (0.329, 1329),
      (0.573, 1593),
      (0.720, 1682),
      (0.585, 1675),
      (0.524, 1487)
    )(getID - 1)
  }
}
