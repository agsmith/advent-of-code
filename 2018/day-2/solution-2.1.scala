import scala.io.Source

val filename = "/Users/asmith986/work/development/advent-of-code/2018/day-2/in.txt"

val lines: List[String]  = Source.fromFile(filename).getLines.toList

def repeats(number: Int, line: String): List[Option[Char]] = {
	line.toList.map { c => 
		if(line.count(_ == c) == number ) Some(c) 
		else None
	}
}

val twos: List[Int] = lines.map { line =>
	repeats(2, line).flatten.toSet.size
}
val threes: List[Int] = lines.map { line =>
	repeats(3, line).flatten.toSet.size
}

twos.filter(_>0).size * threes.filter(_>0).size

