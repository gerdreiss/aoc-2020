val regex: util.matching.Regex = """(\d+)-(\d+) ([a-z]): ([a-z]+)""".r

def part1: Int =
  io.Source
    .fromResource("input")
    .getLines()
    .count { case regex(from, to, char, password) =>
      val count = password.count(_ == char.head)
      from.toInt <= count && count <= to.toInt
    }

def part2: Int =
  io.Source
    .fromResource("input")
    .getLines()
    .count { case regex(idx1, idx2, char, password) =>
      val first  = password.charAt(idx1.toInt - 1)
      val second = password.charAt(idx2.toInt - 1)
      first != second && (first == char.head || second == char.head)
    }

@main def main(): Unit =
  println(part1)
  println(part2)
