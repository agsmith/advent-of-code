import scala.io.Source

val filename = "/Users/asmith986/work/development/advent-of-code/2018/day-3/in.txt"

val lines: List[String]  = Source.fromFile(filename).getLines.toList

case class FromLeftEdge(value: Int)
case class FromTopEdge(value: Int)
case class Wide(value: Int)
case class Tall(value: Int)
case class Location(fromLeftEdge: FromLeftEdge, fromTopEdge: FromTopEdge)
case class Size(wide: Wide, tall: Tall)
case class Claim(id: Int, location: Location, size: Size)
case class Coord(x: Int, y: Int)

type Coverage = List[Coord]

def parseSize(size: String): Size = {
	val ss = size.split("x")
	Size(Wide(ss.head.toInt), Tall(ss.tail.head.toInt))
}

def parseLocation(location: String): Location = {
	val ls = location.split(",")
	Location(FromLeftEdge(ls.head.toInt), FromTopEdge(ls.tail.head.toInt))
}

def parseLine(line: String): Claim = {
    val claimId = line.split("@").toList.head.trim.reverse.dropRight(1).reverse
    val location= line.split("@").tail.mkString.split(":").toList.head.trim
    val size = line.split(":").tail.mkString.trim
    Claim(claimId.toInt, parseLocation(location), parseSize(size))
}

def parseInput(lines: List[String]): List[Claim] = lines.map { parseLine(_) }

def claimCoords(claim: Claim): Coverage = {
// #87 @ 768,216: 20x24
	val startX = 1 + claim.location.fromLeftEdge.value
	val startY = 1000 - claim.location.fromTopEdge.value
	val endX = startX + claim.size.wide.value
	val endY = startY - claim.size.tall.value
	for {
		w <- (startX to endX).toList
		t <- (endY to startY).toList
	} yield Coord(w,t)
}

def claimsCoords(claims: List[Claim]): List[Coverage] = for(claim <- claims) yield claimCoords(claim)

def overlaps(allClaimsCoords: List[Coverage]): List[Int] = allClaimsCoords.combinations(2).collect{ case List(a: Coverage, b: Coverage) if (a intersect b).size > 0 => (a intersect b).size }.toList
  
val claims = parseInput(lines)
val ccs = claimsCoords(claims)
val overlappingCoverages = overlaps(ccs)

overlappingCoverages.foldLeft(0)(_+_)
// overlaps(claimsCoords(parseInput(lines))).foldLeft(0)((a:Int,c:Coverage) => a+c.size)