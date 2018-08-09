import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object dataParser {

  def runCSVParser(): Seq[ArrayBuffer[Int]] = {
    val teamsAA = ArrayBuffer[Int]()
    val teamsBB = ArrayBuffer[Int]()
    val teamAwonn = ArrayBuffer[Int]()

    val bufferedSource = Source.fromFile("nba2015elo.csv")
    for (line <- bufferedSource.getLines) {
      val row = line.split(",").map(_.trim)
      val teamA: Int = row(8) match {
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
        case _ => 31
      }
      val teamB: Int = row(14) match {
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
        case _ => 31
      }
      val teamAwon: Int = row(20) match {
        case "W" => 1
        case "L" => 0
        case _ => 0
      }
      if (teamA != 31 && teamB != 31) {
        teamsAA += teamA
        teamsBB += teamB
        teamAwonn += teamAwon
      }
    }

    bufferedSource.close
    Seq(teamAwonn, teamsAA, teamsBB)

  }

}
