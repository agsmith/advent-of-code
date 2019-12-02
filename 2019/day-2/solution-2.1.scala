import scala.io.Source

val filename = "in.txt"

val input = Source.fromFile(filename).getLines.flatMap(_.split(",")).toList.map(_.toInt).zipWithIndex.map(_.swap).toMap

val DESIRED_OUT = 19690720

def Intcode_computer(ptr: Int, ms: Map[Int, Int]): Int = {
	if(ms.get(ptr).get==99) {
		println("Exiting")
		ms.get(0)
	}
	else if(ms.get(ptr).get==1) {
		println("Updating location "+ ms.get(ms.get(ptr+3).get).get)
		execute(ptr+4, ms.updated(ms.get(ptr+3).get, ms.get(ms.get(ptr+1).get).get+ms.get(ms.get(ptr+2).get).get))
	}
	else if(ms.get(ptr).get==2) {
		println("Updating location "+ ptr+3)
		execute(ptr+4, ms.updated(ms.get(ptr+3).get, ms.get(ms.get(ptr+1).get).get*ms.get(ms.get(ptr+2).get).get))
	}
	else {
		println("Oh noes")
	}
}

val result = Intcode_computer(0, input)