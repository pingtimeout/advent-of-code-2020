package fr.pingtimeout.adventofcode2020

case class Policy(character: Char, times: Range)

case class Day02(mode: Mode) extends AdventOfCode {
  override val dayName: String = "day02"
  lazy val input: Seq[(Policy, String)] = text(mode).map(parseLine)

  def parseLine(line: String): (Policy, String) = {
    val pattern = "^([0-9]+)-([0-9]+) ([a-z]): ([a-z]*)$".r
    line match {
      case pattern(min, max, character, password) => (Policy(character(0), min.toInt to max.toInt), password)
    }
  }

  override def solvePartOne() {
    println(input.count {
      case (Policy(character, range), password) => range.contains(password.count(_ == character))
    })
  }

  override def solvePartTwo() {
    println(input.count {
      case (Policy(character, range), password) =>
        password(range.head - 1) == character ^ password(range.last - 1) == character
    })
  }
}

object Day02 {
  def main(args: Array[String]): Unit = {
    Day02(Example).solvePartOne()
    Day02(Real).solvePartOne()
    Day02(Example).solvePartTwo()
    Day02(Real).solvePartTwo()
  }
}