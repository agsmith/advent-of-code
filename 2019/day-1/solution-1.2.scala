import scala.io.Source
import scala.math._

val filename = "in.txt"

val masses = Source.fromFile(filename).getLines.toList.map(_.toInt)

def fuelsList(ls: List[Int], fuel: Int): List[Int] = {
	if(fuel < 0 ) ls ++ List(0)
	else fuelsList(ls ++ List(fuel), floor(fuel/3).toInt -2)
}

val fuels = 
	masses.
		map(mass => floor(mass/3).toInt - 2).
		map(fuel => fuelsList(List.empty, fuel)).
		flatten.
		fold(0)(_+_)

