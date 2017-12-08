import scala.io.Source

val filename = "/Users/adamsmith/work/ccad/Development/github/advent-of-code/day-7/in.txt"

val lines: List[String]  = Source.fromFile(filename).getLines.toList.map{_.toString.trim}

val listOfChildMaps = for {
  line <- lines
} yield {
  val ss = line.split("->").toList
  val k = ss.head.split("\\(").toList.map{_.toString.trim}.head
  val v = ss.tail.map{_.toString.split(",").toList.map{_.trim}}.flatten
  Map(k -> v )
}

val listOfWeightMaps = for {
  line <- lines
} yield {
  val ss = line.split("->").toList
  val k = ss.head.split("\\(").toList.map{_.toString.trim}.head
  val v = ss.head.split("\\(").toList.map{_.toString.trim}.reverse.head.dropRight(1).toInt
  Map(k -> v )
}

val childMap: Map[String, List[String]] = listOfChildMaps.foldLeft(Map.empty: Map[String, List[String]]){(a,c) => a ++ c}
val weightMap: Map[String, Int] = listOfWeightMaps.foldLeft(Map.empty: Map[String, Int]){(a,c) => a ++ c}
val vs = childMap.values.flatten.toList
val ks = childMap.keys.toList
val rootKey = ks.filter{ k => !vs.contains(k)}.head

def branchWeight(rootKey: String): Int = {
  val childWeights = for{
    child <- childMap.get(rootKey).getOrElse(List.empty)
  } yield {
    branchWeight(child)
  }
  val cw = childWeights.foldLeft(0){_+_}
  // println(s"$cw")
  weightMap.get(rootKey).getOrElse(0) + cw

}

val topChildren = childMap.get(rootKey).getOrElse(List.empty)

topChildren foreach { child =>
  println(s"$child\t${branchWeight(child)}")

}

