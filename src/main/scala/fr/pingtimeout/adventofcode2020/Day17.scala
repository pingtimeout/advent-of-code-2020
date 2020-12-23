package fr.pingtimeout.adventofcode2020

import scala.collection.mutable

case class Day17(mode: Mode) extends AdventOfCode {
  override val dayName: String = "day17"

  type Grid3D = mutable.Map[(Int, Int, Int), Boolean]
  type Grid4D = mutable.Map[(Int, Int, Int, Int), Boolean]

  lazy val initialGrid3D: Grid3D = mutable.Map(
    text(mode).zipWithIndex.flatMap {
      case (line, row) => line.zipWithIndex.map {
        case (character, column) => (row, column, 0) -> (character == '#')
      }
    }: _*)

  lazy val initialGrid4D: Grid4D = mutable.Map(
    text(mode).zipWithIndex.flatMap {
      case (line, row) => line.zipWithIndex.map {
        case (character, column) => (row, column, 0, 0) -> (character == '#')
      }
    }: _*)

  def isActive(grid: Grid3D, x: Int, y: Int, z: Int): Boolean =
    grid.getOrElse((x, y, z), false)

  def isActive(grid: Grid4D, x: Int, y: Int, z: Int, w: Int): Boolean =
    grid.getOrElse((x, y, z, w), false)

  def countActiveNeighboursOf(grid: Grid3D, x: Int, y: Int, z: Int): Int = {
    val activeNeighbours =
      for (row <- (-1 to 1).map(delta => x + delta);
           column <- (-1 to 1).map(delta => y + delta);
           depth <- (-1 to 1).map(delta => z + delta);
           if (row, column, depth) != (x, y, z) && isActive(grid, row, column, depth))
        yield 1
    activeNeighbours.sum
  }

  def countActiveNeighboursOf(grid: Grid4D, x: Int, y: Int, z: Int, w: Int): Int = {
    val activeNeighbours =
      for (row <- (-1 to 1).map(delta => x + delta);
           column <- (-1 to 1).map(delta => y + delta);
           depth <- (-1 to 1).map(delta => z + delta);
           whatever <- (-1 to 1).map(delta => w + delta);
           if (row, column, depth, whatever) != (x, y, z, w) && isActive(grid, row, column, depth, whatever))
        yield 1
    activeNeighbours.sum
  }

  def countActive3DCells(grid: Grid3D): Int = grid.values.count(_ == true)

  def countActive4DCells(grid: Grid4D): Int = grid.values.count(_ == true)

  private def get3DCoordinateRanges(currentGrid: Grid3D, boundaryOffset: Int = 0): (Range, Range, Range) = {
    val xs = currentGrid.keys.map(_._1)
    val ys = currentGrid.keys.map(_._2)
    val zs = currentGrid.keys.map(_._3)
    ((xs.min - boundaryOffset) to (xs.max + boundaryOffset),
      (ys.min - boundaryOffset) to (ys.max + boundaryOffset),
      (zs.min - boundaryOffset) to (zs.max + boundaryOffset))
  }

  private def get4DCoordinateRanges(currentGrid: Grid4D, boundaryOffset: Int = 0): (Range, Range, Range, Range) = {
    val xs = currentGrid.keys.map(_._1)
    val ys = currentGrid.keys.map(_._2)
    val zs = currentGrid.keys.map(_._3)
    val ws = currentGrid.keys.map(_._4)
    ((xs.min - boundaryOffset) to (xs.max + boundaryOffset),
      (ys.min - boundaryOffset) to (ys.max + boundaryOffset),
      (zs.min - boundaryOffset) to (zs.max + boundaryOffset),
      (ws.min - boundaryOffset) to (ws.max + boundaryOffset))
  }

  def next3DState(currentGrid: Grid3D): Grid3D = {
    val (xRange, yRange, zRange) = get3DCoordinateRanges(currentGrid, 1)
    val nextGrid =
      for (x <- xRange;
           y <- yRange;
           z <- zRange;
           cellIsActive = isActive(currentGrid, x, y, z);
           activeNeighbours = countActiveNeighboursOf(currentGrid, x, y, z)
           if (cellIsActive && Seq(2, 3).contains(activeNeighbours) || !cellIsActive && activeNeighbours == 3)
           ) yield (x, y, z) -> true
    mutable.Map(nextGrid: _*)
  }

  def next4DState(currentGrid: Grid4D): Grid4D = {
    val (xRange, yRange, zRange, wRange) = get4DCoordinateRanges(currentGrid, 1)
    val nextGrid =
      for (x <- xRange;
           y <- yRange;
           z <- zRange;
           w <- wRange;
           cellIsActive = isActive(currentGrid, x, y, z, w);
           activeNeighbours = countActiveNeighboursOf(currentGrid, x, y, z, w)
           if (cellIsActive && Seq(2, 3).contains(activeNeighbours) || !cellIsActive && activeNeighbours == 3)
           ) yield (x, y, z, w) -> true
    mutable.Map(nextGrid: _*)
  }

  def print3DState(currentGrid: Grid3D) {
    val (xRange, yRange, zRange) = get3DCoordinateRanges(currentGrid, 0)
    zRange.foreach { z =>
      xRange.foreach { x =>
        println(yRange.map(y => if (isActive(currentGrid, x, y, z)) "#" else ".").mkString)
      }
      println()
    }
  }

  def print4DState(currentGrid: Grid4D) {
    val (xRange, yRange, zRange, wRange) = get4DCoordinateRanges(currentGrid, 0)
    wRange.foreach { w =>
      zRange.foreach { z =>
        println(s"z=${z}, w=${w}")
        xRange.foreach { x =>
          println(yRange.map(y => if (isActive(currentGrid, x, y, z, w)) "#" else ".").mkString)
        }
        println()
      }
    }
  }

  override def doSolvePartOne() {
    val gameOfLife = LazyList.iterate(initialGrid3D)(next3DState)
    println(countActive3DCells(gameOfLife(6)))
  }

  override def doSolvePartTwo() {
    val gameOfLife = LazyList.iterate(initialGrid4D)(next4DState)
    println(countActive4DCells(gameOfLife(6)))
  }
}

object Day17 {
  def main(args: Array[String]): Unit = {
    Day17(Example).solvePartOne()
    Day17(Real).solvePartOne()
    Day17(Example).solvePartTwo()
    Day17(Real).solvePartTwo()
  }
}