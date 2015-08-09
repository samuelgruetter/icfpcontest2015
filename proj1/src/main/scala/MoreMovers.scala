import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object MoreMovers {
  def stateSpaceExplorer(grid: HexGrid): Seq[Move] = {
    trait HowGood
    case object Unreachable extends HowGood
    trait Reachable extends HowGood {
      def lastMove: Step
    }
    case class NonFinal(lastMove: Step) extends Reachable
    case class Final(lastMove: Step, filledRows: Int, emptyWords: Int) extends Reachable

    // space(rotation)(xOffset)(yOffset)
    val space: ArrayBuffer[ArrayBuffer[ArrayBuffer[HowGood]]] =
      ArrayBuffer.fill(6)(ArrayBuffer.fill(grid.width)(ArrayBuffer.fill(grid.height)(Unreachable)))

    case class State(rotation: Int, xOffset: Int, yOffset: Int) {
      lazy val axial = Cell(xOffset, yOffset).toAxial
    }
    // LOL 5 years old Scala bug: https://issues.scala-lang.org/browse/SI-3772
    object StateCompanion {
      def apply(rotation: Int, axial: Cell): State = {
        val Cell(x, y) = axial.toOffset
        State(rotation, x, y)
      }
    }

    def get(s: State): HowGood = {
      space(s.rotation)(s.xOffset)(s.yOffset)
    }
    def set(s: State, h: HowGood): Unit = {
      space(s.rotation)(s.xOffset)(s.yOffset) = h
    }

    def isOnField(s: State): Boolean = {
      s.xOffset >= 0 && s.xOffset < grid.width && s.yOffset >= 0 && s.yOffset < grid.height
    }

    val startState = State(0, grid.unitCenter.toAxial.x, grid.unitCenter.toAxial.y)
    val q: mutable.Queue[State] = mutable.Queue()

    def getLockMove(s: State): Option[Move] = {
      Moves.forwardLocking.find(move => !grid.canPlaceCurrentUnitAt(s.axial.add(move.direction)))
    }

    def isFinal(s: State): Boolean = {
      getLockMove(s).isDefined
    }

    def betterThan(f1: Final, s1: State, f2: Final, s2: State): Boolean = {
      if (f1.filledRows > f2.filledRows) {
        true
      } else if (f1.filledRows == f2.filledRows) {
        if (s1.yOffset > s2.yOffset) {
          true
        } else if (s1.yOffset == s2.yOffset) {
          s1.xOffset > s2.xOffset
        } else {
          false
        }
      } else {
        false
      }
    }

    var bestFinal: Final = null
    var bestState: State = null

    def newState(lastMove: Step, s: State): Unit = {
      if (get(s) == Unreachable) {
        if (isFinal(s)) {
          val f = Final(lastMove, grid.numberOfClearedLinesIfUnitLockedAt(s.axial), 77777)
          set(s, f)
          if (bestFinal == null || betterThan(f, s, bestFinal, bestState)) {
            bestFinal = f
            bestState = s
          }
        } else {
          set(s, NonFinal(lastMove))
        }
        q.enqueue(s)
      }
    }

    newState(null, startState)

    while (q.nonEmpty) {
      val s = q.dequeue()

      Moves.forward.foreach(move => {
        val newPos = s.axial.add(move.direction)
        val newSt = StateCompanion(0, newPos)
        if (isOnField(newSt) && grid.canPlaceCurrentUnitAt(newPos)) {
          newState(move, newSt)
        }
      })
    }

    var res: List[Move] = List(getLockMove(bestState).get)
    var currentState = bestState
    while (get(currentState).asInstanceOf[Reachable].lastMove != null) {
      val forwardMove = get(currentState).asInstanceOf[Reachable].lastMove
      res = forwardMove :: res
      val stepBack = forwardMove.direction.negative
      val prevPos = currentState.axial.add(stepBack)
      currentState = StateCompanion(0, prevPos)
    }

    res.foreach(grid.applyMove)

    res
  }

}
