
sealed abstract class Direction(dxEven: Int, dyEven: Int, dxOdd: Int, dyOdd: Int) {
  def addTo(c: Cell): Cell = {
    if (c.y % 2 == 0) {
      Cell(c.x + dxEven, c.y + dyEven)
    } else {
      Cell(c.x + dxOdd, c.y + dyOdd)
    }
  }
}

object Direction {
  private val all = Iterable(West, East, SouthWest, SouthEast, NorthWest, NorthEast)
  private val forward = Iterable(West, East, SouthWest, SouthEast)
  private val backward = Iterable(West, East, NorthWest, NorthEast)
  private val down = Iterable(SouthWest, SouthEast)

  def allNeighbors(c: Cell): Iterable[Cell] = all.map(_.addTo(c))
  def forwardNeighbors(c: Cell): Iterable[Cell] = forward.map(_.addTo(c))
  def backwardNeighbors(c: Cell): Iterable[Cell] = backward.map(_.addTo(c))
  def downNeighbors(c: Cell): Iterable[Cell] = down.map(_.addTo(c))
}

sealed abstract trait ForwardDirection extends Direction
sealed abstract trait BackwardDirection extends Direction

case object West extends Direction(-1, 0, -1, 0) with ForwardDirection with BackwardDirection
case object East extends Direction( 1, 0,  1, 0) with ForwardDirection with BackwardDirection

case object SouthWest extends Direction(-1, 1, 0, 1) with ForwardDirection
case object SouthEast extends Direction( 0, 1, 1, 1) with ForwardDirection

case object NorthWest extends Direction(-1, -1, 0, -1) with BackwardDirection
case object NorthEast extends Direction( 0, -1, 1, -1) with BackwardDirection
