import scala.io.Source

val filename = "/Users/adamsmith/work/ccad/Development/github/advent-of-code/day-8/in.txt"

val lines: List[String]  = Source.fromFile(filename).getLines.toList.map{_.toString}

val listOfRegMaps: List[Map[String, Int]] = for {
  line <- lines
} yield {
  val string = line.split(" ").toList
  val k = string.head
  Map(k -> 0)
}

val regMap: Map[String, Int] = listOfRegMaps.foldLeft(Map.empty: Map[String, Int]){(a,c) => a ++ c}

case class Command(reg: String, incDec: String, amount: Int, conditionalReg: String, conditional: String, value: Int)

def parseLine(line: String): Command = {
  val strings = line.split(" ").toList
  val value = strings.last
  val conditional = strings.dropRight(1).last
  val conditionalReg = strings.dropRight(2).last
  val amount = strings.dropRight(4).last
  val incDec = strings.dropRight(5).last
  val reg = strings.dropRight(6).last
  Command(reg,incDec, amount.toInt, conditionalReg, conditional, value.toInt)
}

def adjust(c: Command, m: Map[String, Int]): Map[String, Int] = {
  c.incDec match {
    case "inc" =>
      val v = m.get(c.reg).getOrElse(0)
      (m - c.reg) ++ Map(c.reg -> (v + c.amount))
    case "dec" =>
      val v = m.get(c.reg).getOrElse(0)
      (m - c.reg) ++ Map(c.reg -> (v - c.amount))
  }
}

def execute(c: Command, m: Map[String, Int]): Map[String, Int] = {
  c.conditional match {
    case ">=" =>
      if(m.get(c.conditionalReg).getOrElse(0) >= c.value) adjust(c, m)
      else m
    case "<=" =>
      if(m.get(c.conditionalReg).getOrElse(0) <= c.value) adjust(c, m)
      else m
    case ">" =>
      if(m.get(c.conditionalReg).getOrElse(0) > c.value) adjust(c, m)
      else m
    case "<" =>
      if(m.get(c.conditionalReg).getOrElse(0) < c.value) adjust(c, m)
      else m
    case "!=" =>
      if(m.get(c.conditionalReg).getOrElse(0) != c.value) adjust(c, m)
      else m
    case "==" =>
      if(m.get(c.conditionalReg).getOrElse(0) == c.value) adjust(c, m)
      else m
  }
}

def go(lines: List[String], m: Map[String, Int]): Map[String, Int] = {
  if(lines.isEmpty) m
  else {
    val line = lines.head
    val command = parseLine(line)
    val newMap = execute(command, m)
    go(lines.tail, newMap)
  }
}

go(lines, regMap).valuesIterator.max
