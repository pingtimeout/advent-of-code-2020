package fr.pingtimeout.adventofcode2020

case class Day03(mode: Mode) extends AdventOfCode {
  override val dayName: String = "day03"
  lazy val map: IndexedSeq[IndexedSeq[Char]] = text(mode).map(_.toSeq).toIndexedSeq
  lazy val width: Int = map(0).size
  lazy val height: Int = map.size

  def countEncounters(horizontalStep: Int, verticalStep: Int): Long = {
    (0 until (height / verticalStep))
      .map(iteration => (verticalStep * iteration, horizontalStep * iteration % width))
      .map { case (a, b) => if (map(a)(b) == '#') 1 else 0 }
      .sum
      .toLong
  }

  override def solvePartOne() {
    println(countEncounters(3, 1))
  }

  override def solvePartTwo() {
    println(countEncounters(1, 1)
      * countEncounters(3, 1)
      * countEncounters(5, 1)
      * countEncounters(7, 1)
      * countEncounters(1, 2))
  }
}

object Day03 {
  def main(args: Array[String]): Unit = {
    Day03(Example).solvePartOne()
    Day03(Real).solvePartOne()
    Day03(Example).solvePartTwo()
    Day03(Real).solvePartTwo()
  }
}