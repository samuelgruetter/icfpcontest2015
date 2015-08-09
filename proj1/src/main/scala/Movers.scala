import scala.util.Random
import Moves._

object Movers {

  /** Gets a grid with a unit placed at the top, has to produce sequence of moves, including the move that locks the unit */
  type Mover = HexGrid => Seq[Move]

  def betterThanRandomlyDown(grid: HexGrid): Seq[Move] = {
    case class SearchPosition(pos: Cell, prev: Option[SearchPosition], prevMove: Option[Move])

    def possiblePositions(start: SearchPosition): Seq[SearchPosition] = {
      val searchGrid: scala.collection.mutable.Map[(Cell), SearchPosition] = scala.collection.mutable.Map()
      def rec(start: SearchPosition): Seq[SearchPosition] = {
        searchGrid(start.pos) = start

        val posAndMoves = Moves.forward.map(move => (start.pos.add(move.direction), move))
                                       .filter{case (pos, move) => grid.canPlaceCurrentUnitAt(pos)}
        val unvisited = posAndMoves.filter{case (pos, move) => !searchGrid.contains(pos)}

        start +: unvisited.flatMap{case (pos, move) => rec(SearchPosition(pos, Some(start), Some(move)))}.toSeq
      }
      rec(start)
    }
    def score(pos: Cell) = (pos.y, -pos.x)
    def getMoves(searchPos: SearchPosition): Seq[Move] = searchPos.prev match {
      case Some(prev) => getMoves(prev) :+ searchPos.prevMove.get
      case None => Seq()
    }

    val possibleFinalPositions = possiblePositions(SearchPosition(grid.unitCenter, None, None)).filter(
      sp => Directions.all.exists(dir => !grid.canPlaceCurrentUnitAt(sp.pos.add(dir)))
    )
    val best = possibleFinalPositions.maxBy(p => score(p.pos))
    val lockMove = Moves.forwardLocking.find(step => !grid.canPlaceCurrentUnitAt(best.pos.add(step.direction))).get
    val moves = getMoves(best)
    moves.foreach(move => grid.move(move.asInstanceOf[Step].direction))
    grid.lockUnit()
    moves :+ lockMove
  }

  def randomlyDown(grid: HexGrid): Seq[Move] = {
    val rand = new Random(123456789)
    var locked = false
    var trace: List[Move] = Nil
    while (!locked) {
      val moves: List[Step] = if (rand.nextBoolean()) List(SouthWest, SouthEast) else List(SouthEast, SouthWest)
      val possibleMoves = moves.filter(step => grid.canMove(step.direction))
      if (possibleMoves.isEmpty) {
        trace = LockSouthWest :: trace // make a move to lock
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
