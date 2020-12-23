package fr.pingtimeout.adventofcode2020

case class Day05(mode: Mode) extends AdventOfCode {
  override val dayName: String = "day05"
  lazy val input: Seq[Int] = text(mode)
    .map(line => line.replaceAll("[FL]", "0").replaceAll("[BR]", "1"))
    .map(Integer.parseInt(_, 2))
    .sorted

  override def doSolvePartOne() {
    println(input.last)
  }

  override def doSolvePartTwo() {
    println((0 to 1024).diff(input)
      .zip((-1 to 1024).diff(input))
      .filter(x => x._1 - 1 != x._2)
      .map(_._1)
      .head)
  }
}

object Day05 {
  def main(args: Array[String]): Unit = {
    Day05(Example).solvePartOne()
    Day05(Real).solvePartOne()
    Day05(Real).solvePartTwo()
  }
}