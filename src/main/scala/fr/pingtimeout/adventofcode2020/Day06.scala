package fr.pingtimeout.adventofcode2020

case class Day06(mode: Mode) extends AdventOfCode {
  override val dayName: String = "day06"
  lazy val input: Seq[(Int, String)] = text(mode)
    .map(line => if (line.isEmpty) "\n" else line + "#")
    .mkString("")
    .split('\n')
    .map(str => (str.count(_ == '#'), str.replace("#", "")))
    .toSeq

  override def doSolvePartOne() {
    println(input
      .map {
        case (numPersons, allAnswers) => allAnswers.toSet.size
      }.sum)
  }

  override def doSolvePartTwo() {
    println(input
      .map {
        case (numPersons, allAnswers) => allAnswers
          .groupBy(c => c)
          .count { case (char, occurrences) => occurrences.length == numPersons }
      }
      .sum)
  }
}

object Day06 {
  def main(args: Array[String]): Unit = {
    Day06(Example).solvePartOne()
    Day06(Real).solvePartOne()
    Day06(Example).solvePartTwo()
    Day06(Real).solvePartTwo()
  }
}