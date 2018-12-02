import scala.io.Source

val filename = "in.txt"

val lineStrings = Source.fromFile(filename).getLines.toList

val differences = lineStrings.map { lineString =>
  val ints: List[Int] = lineString.split(" ").toList.map {_.toInt }
  val sortedInts = ints.sortWith(_ > _)
  val high = sortedInts.head
  val low = sortedInts.reverse.head
  high-low
}

val checksum = differences.foldLeft(0)(_+_)
