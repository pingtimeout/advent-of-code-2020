package fr.pingtimeout.adventofcode2020

case class Day01(mode: Mode) extends AdventOfCode {
  override val dayName: String = "day01"
  lazy val numbers: IndexedSeq[Int] = text(mode).map(_.toInt).toIndexedSeq

  override def solvePartOne () {
    println({
      for (i <- numbers.indices;
           j <- i + 1 until numbers.size if numbers(i) + numbers(j) == 2020)
        yield numbers(i) * numbers(j)
    }.head)
  }

  override def solvePartTwo() {
    println({
      for (i <- numbers.indices;
           j <- i + 1 until numbers.size;
           k <- j + 1 until numbers.size if numbers(i) + numbers(j) + numbers(k) == 2020)
        yield numbers(i) * numbers(j) * numbers(k)
    }.head)
  }
}

object Day01 {
  def main(args: Array[String]): Unit = {
    Day01(Example).solvePartOne()
    Day01(Example).solvePartTwo()
    Day01(Real).solvePartOne()
    Day01(Real).solvePartTwo()
  }
}
