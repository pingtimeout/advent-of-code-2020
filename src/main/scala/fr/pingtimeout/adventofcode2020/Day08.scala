package fr.pingtimeout.adventofcode2020

case class Day08(mode: Mode) extends AdventOfCode {
  override val dayName: String = "day08"

  lazy val input: Seq[(String, String)] = text(mode)
    .map(line => line.split(" "))
    .map(array => (array(0), array(1)))

  override def doSolvePartOne() {
    @scala.annotation.tailrec
    def runUntilLoopRec(pos: Int, visitedPos: Set[Int], accumulator: Int): Int = {
      if (visitedPos.contains(pos))
        return accumulator
      input(pos) match {
        case ("nop", _) => runUntilLoopRec(pos + 1, visitedPos + pos, accumulator)
        case ("acc", value) => runUntilLoopRec(pos + 1, visitedPos + pos, accumulator + value.toInt)
        case ("jmp", offset) => runUntilLoopRec(pos + offset.toInt, visitedPos + pos, accumulator)
      }
    }

    println(runUntilLoopRec(0, Set(), 0))
  }

  override def doSolvePartTwo() {
    def runUntilLoopRec(pos: Int,
                        visitedPos: Set[Int],
                        accumulator: Int,
                        instructions: Seq[(String, String)]): (Int, Boolean) = {
      if (visitedPos.contains(pos) || pos == instructions.size)
        return (accumulator, pos == instructions.size)
      instructions(pos) match {
        case ("nop", _) => runUntilLoopRec(pos + 1, visitedPos + pos, accumulator, instructions)
        case ("acc", value) => runUntilLoopRec(pos + 1, visitedPos + pos, accumulator + value.toInt, instructions)
        case ("jmp", offset) => runUntilLoopRec(pos + offset.toInt, visitedPos + pos, accumulator, instructions)
      }
    }

    println(input
      .zipWithIndex
      .flatMap {
        case (("jmp" | "nop", offset), index) =>
          Seq(input.updated(index, ("jmp", offset)), input.updated(index, ("nop", offset)))
        case _ => Seq(input)
      }
      .toSet
      .map(instructions => runUntilLoopRec(0, Set(), 0, instructions))
      .flatMap { case (accumulator, success) => if (success) Some(accumulator) else None }
      .head)
  }
}

object Day08 {
  def main(args: Array[String]): Unit = {
    Day08(Example).solvePartOne()
    Day08(Real).solvePartOne()
    Day08(Example).solvePartTwo()
    Day08(Real).solvePartTwo()
  }
}