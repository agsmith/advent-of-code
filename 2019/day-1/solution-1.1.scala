import scala.io.Source
import scala.Math

val filename = "in.txt"

val lineStrings = Source.fromFile(filename).getLines.toList.map(_.toInt)

val result = lineStrings.map(x -> floor(x / 3) - 2).map(_.toInt).fold(0)(_+_)

println result