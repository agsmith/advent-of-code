import scala.io.Source

val filename = "/Users/adamsmith/work/ccad/Development/github/advent-of-code/day-5/in.txt"

val m: Map[Int, Int] = Source.fromFile(filename).getLines.toList.map{_.toInt}.zipWithIndex.map{_.swap}.toMap

def escapeTheBox(i: Int, m: Map[Int, Int], counter: Int): Int = {
  m.get(i) match {
    case None => counter
    case Some(v: Int) => escapeTheBox(i+v, ( m - i ) ++ Map(i -> (v + 1)), counter + 1)
  }
}

escapeTheBox(0,m,0)
