import scala.io.Source

val filename = "/Users/asmith986/work/development/advent-of-code/2018/day-2/in.txt"

val lines: List[String]  = Source.fromFile(filename).getLines.toList


val ls: List[Option[String]] = for {
  line1 <- lines
  line2 <- lines
} yield {
  if((line1 intersect line2).size == (line1.size -1)) Some(line1 ++ " " ++ line2 ++ " " ++ (line1 intersect line2))
  else None
}

ls.flatten

// revtaubfniyhusgxdoajwkqilp 
// revtaubfniyhpsgxdoajwkqilp 
// revtaubfniyh_sgxdoajwkqilp