import scala.math

import scala.collection.immutable.Map

val in = 361527

val square = {
  val x = Math.floor(Math.sqrt(in))
  if(x%2 == 0) x+1
  else x
}

val mid = (square/2)+1

trait Edge
case object Island extends Edge
case object BigIsland extends Edge
case object NECorner extends Edge
case object SECorner extends Edge
case object NWCorner extends Edge
case object SWCorner extends Edge

case object NorthEdge extends Edge
case object SouthEdge extends Edge
case object EastEdge extends Edge
case object WestEdge extends Edge

case class Coords(x: Double, y: Double)

val init = Coords(mid, mid)
val m: Map[Coords, Double] = Map(init -> 1)

def spiral(curr: Coords, i: Double, m: Map[Coords, Double]): Coords = {

  if(i == in)
    curr
  else
    spiral(nextSquare(curr, m), i+1, m ++ Map(curr -> i))

}

def squareEmpty(square: Coords, m: Map[Coords, Double]): Boolean = m.get(square).isEmpty
def EastEmpty(curr: Coords, m: Map[Coords, Double]): Boolean = squareEmpty(Coords(curr.x+1, curr.y), m)
def WestEmpty(curr: Coords, m: Map[Coords, Double]): Boolean = squareEmpty(Coords(curr.x-1, curr.y), m)
def NorthEmpty(curr: Coords, m: Map[Coords, Double]): Boolean = squareEmpty(Coords(curr.x, curr.y+1), m)
def SouthEmpty(curr: Coords, m: Map[Coords, Double]): Boolean = squareEmpty(Coords(curr.x, curr.y-1), m)

def NorthEastEmpty(curr: Coords, m: Map[Coords, Double]): Boolean = squareEmpty(Coords(curr.x+1, curr.y+1), m)
def NorthWestEmpty(curr: Coords, m: Map[Coords, Double]): Boolean = squareEmpty(Coords(curr.x-1, curr.y+1), m)
def SouthEastEmpty(curr: Coords, m: Map[Coords, Double]): Boolean = squareEmpty(Coords(curr.x+1, curr.y-1), m)
def SouthWestEmpty(curr: Coords, m: Map[Coords, Double]): Boolean = squareEmpty(Coords(curr.x-1, curr.y-1), m)

def edge(curr: Coords, m: Map[Coords, Double]): Edge = {

  if(NorthEmpty(curr, m) && EastEmpty(curr, m) && SouthEmpty(curr, m) && WestEmpty(curr, m)) Island
  else if(NorthEmpty(curr, m) && EastEmpty(curr, m) && SouthEmpty(curr, m) && !WestEmpty(curr, m)) BigIsland
  else if(NorthEmpty(curr, m) && WestEmpty(curr, m) && EastEmpty(curr, m) && !SouthEmpty(curr, m) && !SouthWestEmpty(curr, m)) NECorner
  else if(NorthEmpty(curr, m) && WestEmpty(curr, m) && SouthEmpty(curr, m) && !EastEmpty(curr, m) && !SouthEastEmpty(curr, m)) NWCorner
  else if(!NorthEmpty(curr, m)&& EastEmpty(curr, m) && SouthEmpty(curr, m) && WestEmpty(curr, m) && !NorthEastEmpty(curr, m)) SWCorner
  else if(!WestEmpty(curr, m) && NorthEmpty(curr, m)&& SouthEmpty(curr, m) && EastEmpty(curr, m) && !NorthWestEmpty(curr, m)) SECorner
  else if(EastEmpty(curr, m) && !SouthEmpty(curr, m)) EastEdge
  else if(NorthEmpty(curr, m) && !EastEmpty(curr, m)) NorthEdge
  else if(WestEmpty(curr, m) && !NorthEmpty(curr, m)) WestEdge
  else SouthEdge
}

def nextSquare(curr: Coords, m: Map[Coords, Double]): Coords = {
  edge(curr, m) match {
    case Island => Coords(curr.x+1, curr.y)
    case BigIsland => Coords(curr.x, curr.y+1)
    case NECorner => Coords(curr.x-1, curr.y)
    case NWCorner => Coords(curr.x, curr.y-1)
    case SWCorner => Coords(curr.x+1, curr.y)
    case SECorner => Coords(curr.x, curr.y+1)
    case EastEdge => Coords(curr.x, curr.y+1)
    case NorthEdge => Coords(curr.x-1, curr.y)
    case WestEdge => Coords(curr.x, curr.y-1)
    case SouthEdge => Coords(curr.x+1, curr.y)
  }
}

val destCords = spiral(init, 1, m)

def getDiff(i: Double) = {
  if(i < mid)
    mid - i
  else
    i - mid
}

getDiff(destCords.x) + getDiff(destCords.y)

