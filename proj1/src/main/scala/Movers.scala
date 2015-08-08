import scala.util.Random

object Movers {

  /** Gets a grid with a unit placed at the top, has to produce sequence of moves */
  type Mover = HexGrid => Seq[ForwardDirection]

  def randomlyDown(grid: HexGrid): Seq[ForwardDirection] = {
    val rand = new Random()
    var locked = false
    var trace: List[ForwardDirection] = Nil
    while (!locked) {
      val moves = if (rand.nextBoolean()) List(SouthWest, SouthEast) else List(SouthEast, SouthWest)
      val possibleMoves = moves.filter(grid.canMove)
      if (possibleMoves.isEmpty) {
        locked = true
      } else {
        val m = possibleMoves.head
        grid.move(m)
        trace = m :: trace
      }
    }
    trace.reverse
  }

}
