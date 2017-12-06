import scala.collection.immutable.ListMap

val inString = "0 5 10 0 11 14 13 4 11 8 8 7 1 4 12 11"

def sortMap(m: Map[Int, Int]): Map[Int, Int] = ListMap(m.toSeq.sortBy(_._1):_*).toMap

val m: Map[Int, Int] = sortMap(inString.split(" ").toList.map { _.toInt }.zipWithIndex.map{_.swap}.toMap)

case class CounterAndMap(counter: Int, m: Map[Int, Int])

def keyToLargestValue(m: Map[Int, Int]): Int = m.filter{_._2 == m.valuesIterator.max}.keysIterator.min


def redistributeOne(m: Map[Int, Int], k: Int, v: Int): Map[Int,Int] = {
  if(v == 0)
    sortMap(m)
  else {
    val newMap = m.map{ p => if(p._1 == k) (k,p._2 + 1) else p}
    if(k == m.size-1) //wraparound
      redistributeOne(sortMap(newMap), 0, v-1)
    else
      redistributeOne(sortMap(newMap), k+1, v-1)
  }
}

def redistribute(c: CounterAndMap):CounterAndMap = {
    val k = keyToLargestValue(c.m)
    val v = c.m.get(k).get
    val newM: Map[Int, Int] = sortMap((c.m - k) ++ Map (k -> 0))
    val redistMap = {
      if(k==newM.size-1)
        redistributeOne(newM, 0, v)
      else
        redistributeOne(newM, k+1, v)
    }
    CounterAndMap(c.counter+1, sortMap(redistMap))

}

def moves(m: Map[Int, Int], lsss: List[List[Int]], counter: Int): Int = {
  if(lsss.contains(sortMap(m).values.toList)) {
    lsss.foldLeft(0){ (a, v) =>
      if(v==sortMap(m).values.toList || a>0) a+1
      else 0
    }
  }
  else {
    val l = lsss ++ List(m.values.toList)
    val cwl = redistribute(CounterAndMap(counter,m))
    moves(sortMap(cwl.m), l, cwl.counter)
  }
}

moves(m, List(List()), 0)
