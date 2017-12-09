import scala.io.Source

val filename = "/Users/adamsmith/work/ccad/Development/github/advent-of-code/day-7/in.txt"

val lines: List[String]  = Source.fromFile(filename).getLines.toList.map{_.toString.trim}

val childMap: Map[String, List[String]] = line.map { line =>
  val ss = line.split("->").toList
  val k = ss.head.split("\\(").toList.map{_.toString.trim}.head
  val v = ss.tail.map{_.toString.split(",").toList.map{_.trim}}.flatten
  Map(k -> v )
}.foldLeft(Map.empty: Map[String, List[String]]){_++_}

val weightMap: Map[String, Int] = lines.map { line =>
  val ss = line.split("->").toList
  val k = ss.head.split("\\(").toList.map{_.toString.trim}.head
  val v = ss.head.split("\\(").toList.map{_.toString.trim}.reverse.head.dropRight(1).toInt
  Map(k -> v )
}.foldLeft(Map.empty: Map[String, Int]){(a,c) => a ++ c}

val vs = childMap.values.flatten.toList
val ks = childMap.keys.toList
ks.filter{ k => !vs.contains(k)}.head
