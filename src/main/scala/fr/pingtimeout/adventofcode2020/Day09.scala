package fr.pingtimeout.adventofcode2020

case class Day09(mode: Mode, preambleSize: Int) extends AdventOfCode {
  override val dayName: String = "day09"

  lazy val input: Seq[Long] = text(mode).map(_.toLong)

  def isValid(preamble: Seq[Long], number: Long): Boolean =
    preamble.combinations(2).exists(_.sum == number)

  private def findFirstError(): Long = {
    input
      .sliding(preambleSize)
      .zipWithIndex
      .flatMap { case (preamble, index) =>
        if (isValid(preamble, input(index + preambleSize)))
          None
        else
          Some(input(index + preambleSize))
      }
      .next()
  }

  override def doSolvePartOne(): Unit = {
    println(findFirstError())
  }

  override def doSolvePartTwo(): Unit = {
    val error: Long = findFirstError()
    val weaknesses: Seq[Long] =
      for (i <- 0 until input.length;
           j <- (i + 1) until input.length;
           slice <- Seq(input.slice(i, j));
           if slice.sum == error)
        yield
          slice.min + slice.max
    println(weaknesses.head)
  }
}

object Day09 {
  def main(args: Array[String]): Unit = {
    Day09(Example, 5).solvePartOne()
    Day09(Real, 25).solvePartOne()
    Day09(Example, 5).solvePartTwo()
    Day09(Real, 25).solvePartTwo()
  }
}