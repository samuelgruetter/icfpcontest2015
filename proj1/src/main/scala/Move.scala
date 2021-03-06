
trait Move {
  /*
  {p, ', !, ., 0, 3}       move W
  {b, c, e, f, y, 2}       move E
  {a, g, h, i, j, 4}       move SW
  {l, m, n, o, space, 5}   move SE
  {d, q, r, v, z, 1}       rotate clockwise
  {k, s, t, u, w, x}       rotate counter-clockwise
  */
  def toChar: Char = this match {
    case West => '3'
    case East => '2'
    case SouthWest => '4'
    case SouthEast => '5'
    case RotateClockwise => 'd'
    case RotateCounterclockwise => 'k'
    case LockWest => 'p'
    case LockEast => 'b'
    case LockSouthWest => 'a'
    case LockSouthEast => 'l'
  }
}

object Move {
  def fromChar(c: Char): Move = c match {
    case '3' => West
    case '2' => East
    case '4' => SouthWest
    case '5' => SouthEast
    case 'd' => RotateClockwise
    case 'k' => RotateCounterclockwise
    case 'p' => LockWest
    case 'b' => LockEast
    case 'a' => LockSouthWest
    case 'l' => LockSouthEast
  }
}

trait Rotation extends Move {
  def isClockwise: Boolean = this match {
    case RotateClockwise => true
    case RotateCounterclockwise => false
  }
}

class Step(val direction: Cell, val locks: Boolean) extends Move

case object RotateClockwise extends Rotation
case object RotateCounterclockwise extends Rotation

case object West extends Step(Cell(-1, 0), false)
case object East extends Step(Cell( 1, 0), false)
case object SouthWest extends Step(Cell( 0,  1), false)
case object SouthEast extends Step(Cell( 1,  1), false)
case object NorthWest extends Step(Cell(-1, -1), false)
case object NorthEast extends Step(Cell( 0, -1), false)

case object LockWest extends Step(Cell(-1, 0), true)
case object LockEast extends Step(Cell( 1, 0), true)
case object LockSouthWest extends Step(Cell( 0,  1), true)
case object LockSouthEast extends Step(Cell( 1,  1), true)
case object LockNorthWest extends Step(Cell(-1, -1), true)
case object LockNorthEast extends Step(Cell(-1, -1), true)

object Moves {
  val all = Iterable(West, East, SouthWest, SouthEast, NorthWest, NorthEast)
  val forward = Iterable(West, East, SouthWest, SouthEast)
  val forwardLocking = Iterable(LockWest, LockEast, LockSouthWest, LockSouthEast)
  val backward = Iterable(West, East, NorthWest, NorthEast)
  val down = Iterable(SouthWest, SouthEast)
}
