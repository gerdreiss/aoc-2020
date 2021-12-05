val byrR   = """byr:(\d{4})""".r
val iyrR   = """iyr:(\d{4})""".r
val eyrR   = """eyr:(\d{4})""".r
val hgtCmR = """hgt:(\d{3})cm""".r
val hgtInR = """hgt:(\d{2})in""".r
val hclR   = """hcl:(#[0-9a-f]{6})""".r
val eclR   = """ecl:(amb|blu|brn|gry|grn|hzl|oth)""".r
val pidR   = """pid:([0-9]{9})""".r

val fields = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

@main def main(): Unit =
  val result = io.Source
    .fromResource("input.txt")
    .mkString
    .split("\n\n")
    .count { pass =>
      // part one:
      // fields
      //   .map(_ + ":")
      //   .forall(pass.contains)
      // part two:
      pass
        .split("\\s+")
        .foldLeft(0) { (acc, s) =>
          s match
            case byrR(year) if (1920 to 2002).contains(year.toInt)     => acc + 1
            case iyrR(year) if (2010 to 2020).contains(year.toInt)     => acc + 1
            case eyrR(year) if (2020 to 2030).contains(year.toInt)     => acc + 1
            case hgtCmR(height) if (150 to 193).contains(height.toInt) => acc + 1
            case hgtInR(height) if (59 to 76).contains(height.toInt)   => acc + 1
            case hclR(_)                                               => acc + 1
            case eclR(_)                                               => acc + 1
            case pidR(_)                                               => acc + 1
            case _                                                     => acc
        } == fields.length
    }
  println(result)
