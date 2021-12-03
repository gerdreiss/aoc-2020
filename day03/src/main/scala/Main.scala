val lines: List[String] =
  io.Source
    .fromResource("input.txt")
    .getLines()
    .toList

def countTrees(lines: List[String], right: Int, down: Int): Int =
  lines
    .drop(down)
    .zipWithIndex
    .filter((_, idx) => idx % down == 0)
    .map(_._1)
    .foldLeft((0, right)) { case ((trees, x), line) =>
      val newTrees = if line.charAt(x) == '#' then trees + 1 else trees
      val newX     = (x + right) % line.length
      (newTrees, newX)
    }
    ._1

def checkSlopes(lines: List[String], slopes: List[(Int, Int)]): List[Long] =
  slopes.map(slope => countTrees(lines, slope._1, slope._2))

@main def main(): Unit =
  println(
    checkSlopes(
      lines,
      List(
        (1, 1),
        (3, 1),
        (5, 1),
        (7, 1),
        (1, 2)
      )
    ).product
  )
