package solutions.day6

object DaySix extends App {

  val input = "Time:        48     93     85     95\nDistance:   296   1928   1236   1391"
  val test = "Time:      7  15   30\nDistance:  9  40  200"
  def parsePt1(s: String) = {
    val regex = """Time:([\s\d]+)\nDistance:([\s\d]+)""".r
    def pluck(s: String) = s.split("\\s+").filter(_.nonEmpty).map(_.toLong)
    s match {
      case regex(times, distances) => pluck(times).zip(pluck(distances)).toSeq
    }
  }

  def parsePt2(s: String) = {
    val regex = """Time:([\s\d]+)\nDistance:([\s\d]+)""".r

    def pluck(s: String) = s.replaceAll(" ", "").toLong

    s match {
      case regex(times, distances) => Seq(pluck(times) -> pluck(distances))
    }
  }

  def calculateRace(time: Long, recordDistance: Long) = {
    Seq.range(0, time).count { holdDownSpeed =>
      val runTime = time - holdDownSpeed
      val distance = runTime * holdDownSpeed
      distance > recordDistance
    }
  }

  val runWith = test
  println(parsePt1(runWith).map{case (t, d) => calculateRace(t, d)}.product)
  println(parsePt2(runWith).map{case (t, d) => calculateRace(t, d)}.product)
}
