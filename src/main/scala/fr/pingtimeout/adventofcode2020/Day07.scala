package fr.pingtimeout.adventofcode2020

import scala.util.matching.Regex

case class Day07(mode: Mode) extends AdventOfCode {
  override val dayName: String = "day07"
  val specification: Regex = "^(.+?) bags contain (.*)\\.$".r
  val contents: Regex = "^(\\d+?) (.*) bags?\\.?$".r

  // @formatter:off
  case class Bag(name: String)
  case class Quantity(value: Long)
  // @formatter:on

  lazy val bagsToContent: Map[Bag, Map[_ <: Bag, Quantity]] = text(mode)
    .map {
      case specification(containerBag, "no other bags") => (Bag(containerBag), Map())
      case specification(containerBag, contents) =>
        (Bag(containerBag), contents
          .split(", ")
          .map({ case contents(quantity, containedBag) => Bag(containedBag) -> Quantity(quantity.toLong) })
          .toMap)
    }
    .toMap

  lazy val bagsToContainer: Seq[(Bag, Bag)] =
    bagsToContent
      .toList
      .flatMap {
        case (containerBag, content) => content
          .keys
          .map((_, containerBag))
      }

  def listBagsContainersOf(bag: Bag): List[Bag] = {
    bagsToContainer
      .flatMap {
        case (containedBag, containerBag) if (containedBag == bag) => containerBag :: listBagsContainersOf(containerBag)
        case _ => None
      }
      .toList
  }

  def countBagsContainedBy(containerBag: Bag): Long = {
    if (bagsToContent(containerBag).isEmpty) {
      1
    } else {
      1 + bagsToContent(containerBag)
        .map { case (containedBag, quantity) => quantity.value * countBagsContainedBy(containedBag) }
        .sum
    }
  }

  override def solvePartOne() {
    println(listBagsContainersOf(Bag("shiny gold")).toSet.size)
  }

  override def solvePartTwo(): Unit = {
    println(countBagsContainedBy(Bag("shiny gold")) - 1)
  }
}

object Day07 {
  def main(args: Array[String]): Unit = {
    Day07(Example).solvePartOne()
    Day07(Real).solvePartOne()
    Day07(Example).solvePartTwo()
    Day07(Real).solvePartTwo()
  }
}