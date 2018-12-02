import scala.io.Source

case class OpVal(operator: String, value: Int)

val filename = "/Users/asmith986/work/development/advent-of-code/2018/day-1/in.txt"

val lines: List[String]  = Source.fromFile(filename).getLines.toList

val fs: List[Int => Int] = lines.map(s => {
  val int = s.tail.toInt
  val f: Int => Int = s.head match {
    case '+' => _ + int
    case '-' => _ - int
  }
  f
})


val stream = Stream.continually(fs).flatten

def findFirstDouble(sum: Int = 0, set: Set[Int] = Set.empty, stream: Stream[Int => Int] = stream): Int = {
  if(set.contains(sum))
    sum
  else
    findFirstDouble(stream.head(sum), set + sum, stream.tail)
}

findFirstDouble()