import scala.io.Source


case class OpVal(operator: String, value: Int)

val filename = "/Users/asmith986/work/development/advent-of-code/2018/day-1/in.txt"

val lines: List[String]  = Source.fromFile(filename).getLines.toList

val opValList: List[OpVal] = 
	lines.
		map(_.toList).
		map(s=> OpVal(s.head.toString, s.tail.mkString("").toInt))

val finalFrequency: Int = opValList.foldLeft(0){ (accumulator: Int, ov: OpVal) =>
	ov.operator match {
		case "+" => accumulator + ov.value
		case "-" => accumulator - ov.value
		case _ => accumulator - ov.value
	}

}