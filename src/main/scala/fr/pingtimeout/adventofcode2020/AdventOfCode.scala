package fr.pingtimeout.adventofcode2020

import scala.io.Source

trait AdventOfCode {
  val dayName: String
  lazy val exampleText = Source.fromResource(s"${dayName}/example.txt").getLines.toList
  lazy val inputText = Source.fromResource(s"${dayName}/real.txt").getLines.toList

  def text(mode: Mode): Seq[String] =
    mode match {
      case Example => exampleText
      case Real => inputText
    }

  def solvePartOne(): Unit = ???

  def solvePartTwo(): Unit = ???
}

trait Mode

case object Example extends Mode

case object Real extends Mode