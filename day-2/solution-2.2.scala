import scala.io.Source

val filename = "in.txt"

val lineStrings = Source.fromFile(filename).getLines.toList

val divs = for {
  lineString <- lineStrings
  ints: List[Int] = lineString.split(" ").toList.map {_.toInt }
  x <- ints
  y <- ints
  if (x > y) && (x%y == 0)
} yield x/y

val checksum = divs.foldLeft(0)(_+_)
