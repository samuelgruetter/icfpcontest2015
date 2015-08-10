
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

  def undo: Move
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

abstract class Step(val direction: Cell, val locks: Boolean) extends Move
abstract class NormalStep(direction: Cell) extends Step(direction, false)
abstract class LockStep(direction: Cell) extends Step(direction, true) {
  def undo = throw new UnsupportedOperationException
}

case object RotateClockwise extends Rotation {
  def undo = RotateCounterclockwise
}
case object RotateCounterclockwise extends Rotation {
  def undo = RotateClockwise
}

case object West extends NormalStep(Cell(-1, 0)) {
  def undo = East
}
case object East extends NormalStep(Cell( 1, 0)) {
  def undo = West
}
case object SouthWest extends NormalStep(Cell( 0,  1)) {
  def undo = NorthEast
}
case object SouthEast extends NormalStep(Cell( 1,  1)) {
  def undo = NorthWest
}
case object NorthWest extends NormalStep(Cell(-1, -1)) {
  def undo = SouthEast
}
case object NorthEast extends NormalStep(Cell( 0, -1)) {
  def undo = SouthWest
}

case object LockWest extends LockStep(Cell(-1, 0))
case object LockEast extends LockStep(Cell( 1, 0))
case object LockSouthWest extends LockStep(Cell( 0,  1))
case object LockSouthEast extends LockStep(Cell( 1,  1))
case object LockNorthWest extends LockStep(Cell(-1, -1))
case object LockNorthEast extends LockStep(Cell(-1, -1))

object Moves {
  val all = Iterable(West, East, SouthWest, SouthEast, NorthWest, NorthEast)
  val forward = Iterable(West, East, SouthWest, SouthEast)
  val forwardWithRotation = Iterable(RotateClockwise, RotateCounterclockwise, West, East, SouthWest, SouthEast)
  val forwardLocking = Iterable(LockWest, LockEast, LockSouthWest, LockSouthEast)
  val backward = Iterable(West, East, NorthWest, NorthEast)
  val down = Iterable(SouthWest, SouthEast)
}
