import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object dataParser {

  def runCSVParser(): Seq[ArrayBuffer[Int]] = {
    val teamsA = ArrayBuffer[Int]()
    val teamsB = ArrayBuffer[Int]()
    val teamsAwon = ArrayBuffer[Int]()

    val bufferedSource = Source.fromFile("nba20172018.csv")

    bufferedSource.getLines().drop(1).foreach { line =>
      val row = line.split(",").map(_.trim)
      val (teamA, teamB) = (team(row(2).split(" vs. | @ ").head),
                            team(row(2).split(" vs. | @ ").last))
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
