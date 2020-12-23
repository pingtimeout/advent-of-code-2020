package fr.pingtimeout.adventofcode2020

case class Day04(mode: Mode, part: Option[String] = None) extends AdventOfCode {
  override val dayName: String = "day04"
  lazy val input: Seq[Map[String, String]] = text(mode)
    .map(line => if (line.isEmpty) "\n" else line + " ")
    .mkString("")
    .split('\n')
    .flatMap(line =>
      if (part.isEmpty) Some(line)
      else if (line.contains(part.get)) Some(line.replace(part.get, ""))
      else None)
    .map(passport => passport.split(' ')
      .map(group => group.split(':'))
      .map(group => group(0) -> group(1))
      .sorted
      .toMap)
    .toSeq

  override def doSolvePartOne() {
    println(input
      .map(passport => Seq("byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid").forall(passport.contains))
      .map(if (_) 1 else 0)
      .sum)
  }

  override def doSolvePartTwo() {
    val validationRules: Map[String, String => Boolean] = Map(
      "byr" -> (str => str.length == 4 && (1920 to 2002).contains(str.toInt)),
      "iyr" -> (str => str.length == 4 && (2010 to 2020).contains(str.toInt)),
      "eyr" -> (str => str.length == 4 && (2020 to 2030).contains(str.toInt)),
      "hgt" -> (str => "^1([5-8][0-9]|9[0-3])cm|(59|6[0-9]|7[0-6])in$".r.matches(str)),
      "hcl" -> (str => "^#[0-9a-f]{6}$".r.matches(str)),
      "ecl" -> (str => Seq("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(str)),
      "pid" -> (str => "^[0-9]{9}$".r.matches(str)),
    )

    println(input
      .filter(passport => Seq("byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid").forall(passport.contains))
      .count(passport => validationRules.forall {
        case (field, rule) => rule(passport(field))
      }))
  }
}

object Day04 {
  def main(args: Array[String]): Unit = {
    Day04(Example, Some("PART1")).solvePartOne()
    Day04(Real).solvePartOne()
    Day04(Example, Some("PART2")).solvePartTwo()
    Day04(Real).solvePartTwo()
  }
}