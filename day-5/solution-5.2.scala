import scala.io.Source

val filename = "in.txt"

val ints = Source.fromFile(filename).getLines.toList.map{_.toInt}

val m: Map[Int, Int] = ints.zipWithIndex.map{_.swap}.toMap

def escapeTheBox(i: Int, m: Map[Int, Int], counter: Int): Int = {
  m.get(i) match {
    case None => counter
    case Some(v: Int) if v > 2 =>
      val newIndex: Int = i + v
      val newValue = v - 1
      val newMap: Map[Int, Int] = ( m - i ) ++ Map(i -> newValue)
      escapeTheBox(newIndex, newMap, counter + 1)
    case Some(v: Int) if v < 3 =>
      val newIndex: Int = i + v
      val newValue = v + 1
      val newMap: Map[Int, Int] = ( m - i ) ++ Map(i -> newValue)
      escapeTheBox(newIndex, newMap, counter + 1)
  }
}

escapeTheBox(0,m,0)
