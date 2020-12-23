package fr.pingtimeout.adventofcode2020

import java.util.concurrent.TimeUnit

import scala.io.Source

trait AdventOfCode {
  val dayName: String
  lazy val exampleText: Seq[String] = Source.fromResource(s"${dayName}/example.txt").getLines.toList
  lazy val inputText: Seq[String] = Source.fromResource(s"${dayName}/real.txt").getLines.toList

  def text(mode: Mode): Seq[String] =
    mode match {
      case Example => exampleText
      case Real => inputText
    }

  final def time(function: => Unit): Unit = {
    val start = System.nanoTime()
    val result = function // call-by-name
    val end = System.nanoTime()
    val elapsed = end - start
    println(s"Elapsed time: ${TimeUnit.NANOSECONDS.toMillis(elapsed)}ms")
    println()
  }

  // @formatter:off
  final def solvePartOne() = time(doSolvePartOne())
  final def solvePartTwo() = time(doSolvePartTwo())
  def doSolvePartOne(): Unit = ???
  def doSolvePartTwo(): Unit = ???
  // @formatter:on
}

// @formatter:off
sealed trait Mode
case object Example extends Mode
case object Real extends Mode
// @formatter:on