import scala.util.Random
import Moves._

object Movers {

  /** Gets a grid with a unit placed at the top, has to produce sequence of moves, including the move that locks the unit */
  type Mover = HexGrid => Seq[Move]

  def randomlyDown(grid: HexGrid): Seq[Move] = {
    val rand = new Random(123456789)
    var locked = false
    var trace: List[Move] = Nil
    while (!locked) {
      val moves: List[Step] = if (rand.nextBoolean()) List(SouthWest, SouthEast) else List(SouthEast, SouthWest)
      val possibleMoves = moves.filter(step => grid.canMove(step.direction))
      if (possibleMoves.isEmpty) {
        trace = SouthWest :: trace // make a move to lock
        grid.lockUnit()
        locked = true
      } else {
        val m = possibleMoves.head
        grid.move(m.direction)
        trace = m :: trace
      }
    }
    trace.reverse
  }

}
