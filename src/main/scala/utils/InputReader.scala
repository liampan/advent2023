package utils

import scala.io.Source

object InputReader {

  def getInput(fileName: String): List[String] = {
    val source = Source.fromFile("./src/main/scala/inputs/" + fileName)
    val lines = try source.getLines().toList finally source.close()
    lines
  }
}
