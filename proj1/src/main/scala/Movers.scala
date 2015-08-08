import scala.util.Random
import Moves._

object Movers {

  /** Gets a grid with a unit placed at the top, has to produce sequence of moves */
  type Mover = HexGrid => Seq[Move]

  def randomlyDown(grid: HexGrid): Seq[Move] = {
    val rand = new Random()
    var locked = false
    var trace: List[Move] = Nil
    while (!locked) {
      val moves: List[Step] = if (rand.nextBoolean()) List(SouthWest, SouthEast) else List(SouthEast, SouthWest)
      val possibleMoves = moves.filter(step => grid.canMove(step.direction))
      if (possibleMoves.isEmpty) {
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
