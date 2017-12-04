import scala.io.Source

val filename = "/Users/adamsmith/work/ccad/Development/github/advent-of-code/day-4/in.txt"

val lineStrings = Source.fromFile(filename).getLines.toList

val valid = lineStrings.filter { lineString =>
  val strings: List[String] = lineString.split(" ").toList
  val sortedStrings = strings.map { s =>
    val deconstructed = s.split("").toList.sortWith(_>_)
    deconstructed.foldLeft("")(_++_)
  }
  val stringSet = sortedStrings.toSet
  stringSet.size == strings.size
}

valid.size
