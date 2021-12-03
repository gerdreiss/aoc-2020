def productOfN(n: Int): Int =
  io.Source
    .fromResource("input")
    .getLines()
    .map(_.toInt)
    .toSeq
    .combinations(n)
    .filter(_.sum == 2020)
    .map(_.product)
    // we know there should be exactly one result,
    // therefore sum will give us that result
    .sum

@main def main(): Unit =
  println(productOfN(2))
  println(productOfN(3))
