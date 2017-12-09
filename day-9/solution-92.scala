import scala.io.Source

val filename = "/Users/adamsmith/work/ccad/Development/github/advent-of-code/day-9/in.txt"

val chars: List[String]  = Source.fromFile(filename).getLines.toList.map{_.toString}.head.split("").toList

def countGarbage(isGarbage: Boolean, garbageCount: Int): Int = {
  if(isGarbage) garbageCount+1
  else garbageCount
}

def howMuchGarbage(remainingList: List[String], garbageCount: Int = 0, cleanList: List[String] = List.empty, isGarbage: Boolean = false, isEscaped: Boolean = false): Int = {
  if(remainingList.isEmpty) garbageCount
  else {
    remainingList.head match {
      case "{" if (!isGarbage && !isEscaped) => howMuchGarbage(remainingList.tail, countGarbage(isGarbage && !isEscaped, garbageCount), cleanList ++ List("{"), isGarbage)
      case "}" if (!isGarbage && !isEscaped) => howMuchGarbage(remainingList.tail, countGarbage(isGarbage && !isEscaped, garbageCount), cleanList ++ List("}"), isGarbage)
      case "<" if (!isGarbage && !isEscaped) => howMuchGarbage(remainingList.tail, countGarbage(false, garbageCount), cleanList, true)
      case ">" if (isGarbage && !isEscaped) => howMuchGarbage(remainingList.tail, countGarbage(false, garbageCount), cleanList, false)
      case "!" if !isEscaped => howMuchGarbage(remainingList.tail, countGarbage(false, garbageCount), cleanList, isGarbage, true)
      case _ if !isEscaped => howMuchGarbage(remainingList.tail, countGarbage(isGarbage && !isEscaped,garbageCount), cleanList, isGarbage)
      case _ => howMuchGarbage(remainingList.tail, countGarbage(isGarbage && !isEscaped, garbageCount), cleanList, isGarbage)
    }
  }
}

howMuchGarbage(chars)
