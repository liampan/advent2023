package solutions.day1

import utils.InputReader

object DayOne extends App {

  private val input = InputReader.getInput("day1/input.txt")

  val test: Seq[String] = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen".split("\n").toSeq

  def correctWordsToNums(s: String) = {
    s
      .replaceAll("one", "one1one")
      .replaceAll("two", "two2two")
      .replaceAll("three", "three3three")
      .replaceAll("four", "four4four")
      .replaceAll("five", "five5five")
      .replaceAll("six", "six6six")
      .replaceAll("seven", "seven7seven")
      .replaceAll("eight", "eight8eight")
      .replaceAll("nine", "nine9nine")
  }

  def extractCalibrationValues(seq: Seq[String]) = {
    seq.map{ line =>
      val digitLine = correctWordsToNums(line)
      val digitString = digitLine.find(_.isDigit).getOrElse("").toString + digitLine.findLast(_.isDigit).getOrElse("")
      digitString.toInt
    }.sum
  }

  println(extractCalibrationValues(input))

}
