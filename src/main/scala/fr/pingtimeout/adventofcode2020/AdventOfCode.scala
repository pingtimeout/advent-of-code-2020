package fr.pingtimeout.adventofcode2020

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

  def solvePartOne(): Unit = ???

  def solvePartTwo(): Unit = ???
}

// @formatter:off
sealed trait Mode
case object Example extends Mode
case object Real extends Mode
// @formatter:on