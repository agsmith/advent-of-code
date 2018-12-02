import scala.io.Source

val filename = "/Users/adamsmith/work/ccad/Development/github/advent-of-code/day-9/in.txt"

val chars: List[String]  = Source.fromFile(filename).getLines.toList.map{_.toString}.head.split("").toList

def test: Unit = {

  val one: List[String] = List("{","{", "}","{", "}", "}")
  val two: List[String] = "{{{},{},{{}}}}".split("").toList
  val three: List[String] = "{<a>,<a>,<a>,<a>}".split("").toList
  val four: List[String] = "{{<ab>},{<ab>},{<ab>},{<ab>}}".split("").toList
  val five: List[String] = "{{<!!>},{<!!>},{<!!>},{<!!>}}".split("").toList
  val six: List[String] = "{{<a!>},{<a!>},{<a!>},{<ab>}}".split("").toList

  if(nestDepth(howMuchGarbage(one)) == 5 && nestDepth(howMuchGarbage(two)) == 16 && nestDepth(howMuchGarbage(three)) == 1 && nestDepth(howMuchGarbage(four)) == 9 && nestDepth(howMuchGarbage(five)) == 9 && nestDepth(howMuchGarbage(six)) == 3)
    println("ALL TESTS PASS")
  else
    println("TESTS FAILING")

}

def howMuchGarbage(remainingList: List[String], cleanList: List[String] = List.empty, isGarbage: Boolean = false, isEscaped: Boolean = false): List[String] = {
  if(remainingList.isEmpty) cleanList
  else {
    remainingList.head match {
      case "{" if (!isGarbage && !isEscaped) => howMuchGarbage(remainingList.tail, cleanList ++ List("{"), isGarbage)
      case "}" if (!isGarbage && !isEscaped) => howMuchGarbage(remainingList.tail, cleanList ++ List("}"), isGarbage)
      case "<" if (!isGarbage && !isEscaped) => howMuchGarbage(remainingList.tail, cleanList, true)
      case ">" if (isGarbage && !isEscaped) => howMuchGarbage(remainingList.tail, cleanList, false)
      case "!" if !isEscaped => howMuchGarbage(remainingList.tail, cleanList, isGarbage, true)
      case _ if !isEscaped => howMuchGarbage(remainingList.tail, cleanList, isGarbage)
      case _ => howMuchGarbage(remainingList.tail, cleanList, isGarbage)
    }
  }
}

def nestDepth(l: List[String], depth: Int = 0, v: Int = 0): Int = {
  if(l.isEmpty) v
  else {
    l.head match {
      case "{" => nestDepth(l.tail, depth+1, v)
      case "}" => nestDepth(l.tail, depth-1, v+depth)
    }
  }

}

test

nestDepth(howMuchGarbage(chars))

