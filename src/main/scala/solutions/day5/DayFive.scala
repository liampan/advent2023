package solutions.day5


import utils.InputReader


object DayFive extends App {

  val input = InputReader.getInput("day5/input.txt")
  val test = "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4".split("\n")

  case class SeedRange(destination: Long, source: Long, range: Long) {
    def contains(input: Long) =
      input >= source && input <= source + range

    def backwards(output: Long) = {
      output >= destination && output <= destination+range
    }
  }

  case class SeedMap(name: String, seq: Seq[SeedRange]) {
    def corresponds(input: Long) =
      seq.find(_.contains(input)).map(sr => sr.destination + (input - sr.source)).getOrElse(input)

    def cameFrom(output: Long) = {
      seq.find(_.backwards(output)).map(sr => output - sr.destination + sr.source).getOrElse(output)
    }
  }
  case class Almanac(seeds: Seq[Long], seedMaps: Seq[SeedMap]) {
    private def locateSeed(seed: Long) =
      seedMaps.foldLeft(seed)((acc, map) => map.corresponds(acc))

    private def workBackwardsFrom(location: Long) =
      seedMaps.reverse.foldLeft(location)((acc, map) => map.cameFrom(acc))

    def partOne(): (Long, Long) = seeds.map(s => (s, locateSeed(s))).minBy(_._2)

    def partTwo() = {
      val seedRanges = seeds.grouped(2).toSeq.map{
        case Seq(init, range) => (init, init+range)
      }
      def tryLocation(location: Long): Long = {
        val potentialSeed = workBackwardsFrom(location)
        if (seedRanges.exists {case (small, big) => potentialSeed >= small && potentialSeed <= big}) location
        else tryLocation(location + 1)
      }
      tryLocation(1)
    }
  }

  object Almanac{
    def apply(seq: Seq[String]) = {
      val seeds = seq.head.split(" ").tail.map(_.toLong)

      val maps = seq.tail.mkString("\n").split("\n\n")
        .map{map =>
          val cleanedMap = map.split("\n").toList.filter(_.nonEmpty)
          val ranges = cleanedMap.tail.map(
            _.split(" ").toList.map(_.toLong) match {
              case List(a, b, c) => SeedRange(a, b, c)
            }
          )
          SeedMap(cleanedMap.head.split(" ").head, ranges)
        }
      new Almanac(seeds, maps)
    }
  }


  val (location) = Almanac(input).partTwo()
  println(location)



}