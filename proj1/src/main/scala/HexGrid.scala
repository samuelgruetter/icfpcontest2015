import java.io.PrintStream
import scala.collection.mutable.ArrayBuffer


sealed abstract class GridCell {
  def toChar: Char
}

case object EmptyCell extends GridCell {
  def toChar = '.'
}

/*
case class UnitCell(unitt: Unitt) extends GridCell {
  def toChar = '0'
}
case object UnitCell extends GridCell {
  def toChar = 'u'
}
*/

case object FullCell extends GridCell {
  def toChar = 'X'
}

object HexGrid {
  // initialize with raw problem in offset coordinates
  def apply(prob: Problem): HexGrid = {
    val g = new HexGrid(prob.width, prob.height)
    for (pos <- prob.filled.map(_.toAxial)) g.setCell(pos, FullCell)
    g
  }
}

class HexGrid(val width: Int, val height: Int) {
  // in offset coordinates
  private val grid: ArrayBuffer[ArrayBuffer[GridCell]] =
    ArrayBuffer.fill(width)(ArrayBuffer.fill(height)(EmptyCell))

  // get and set cells in axial coordinates
  def cell(pos: Cell): GridCell = {
    val offset = pos.toOffset
    grid(offset.x)(offset.y)
  }
  def setCell(pos: Cell, c: GridCell): Unit = {
    val offset = pos.toOffset
    grid(offset.x)(offset.y) = c
  }

  // in axial coordinates
  var unitCenter: Cell = Cell(-1000000000, -1000000000)

  // in axial coordinates
  var unit: Unitt = Unitt.empty

  def currentUnitPivot: Cell = unitCenter.add(unit.pivot)

  /** places u on the grid such that u's local (0,0) is at (x0,y0)
  def placeUnitAt(u: Unitt, x0: Int, y0: Int): Unit = {
    for (Cell(x, y) <- u.members) setCell(x0 + x, y0 + y, UnitCell)
  }*/

  /** returns false for cells outside grid */
  def isCellEmpty(pos: Cell): Boolean = {
    val offset = pos.toOffset
    offset.x >= 0 && offset.x < width && offset.y >= 0 && offset.y < height && cell(pos) == EmptyCell
  }

  def canPlaceCurrentUnitAt(p0: Cell): Boolean = {
    unit.members.forall(c => isCellEmpty(p0.add(c)))
  }

  def canMove(dir: Cell): Boolean = {
    canPlaceCurrentUnitAt(unitCenter.add(dir))
  }

  def move(dir: Cell): Unit = {
    unitCenter = unitCenter.add(dir)
  }

  // assumes offset coordinates
  def spawnUnit(u: Unitt): Unit = {
    val xLeftRel = u.leftmost
    val xRightRel = u.rightmost
    val w = xRightRel - xLeftRel + 1
    val xLeftAbs = (width - w) / 2
    val xCenterAbs = xLeftAbs - xLeftRel

    val yCenterAbs = -u.topmost

    unitCenter = Cell(xCenterAbs, yCenterAbs).toAxial
    unit = u.toAxial
  }

  def lockUnit(): Unit = {
    unit.members.foreach(c => setCell(c.add(unitCenter), FullCell))
    unit = Unitt.empty
  }

  def printTo(ps: PrintStream): Unit = {
    for (y <- 0 until height) {
      if (y % 2 != 0) ps.print(" ")
      for (x <- 0 until width) {
        ps.print(cellToChar(Cell(x, y).toAxial))
        ps.print(" ")
      }
      ps.println()
    }
  }

  def isCoveredByUnit(pos: Cell): Boolean = {
    // members are in relative coordinates
    unit.members.contains(pos.add(unitCenter.negative))
  }

  def cellToChar(pos: Cell): Char = {
    if (pos == currentUnitPivot) {
      if (isCoveredByUnit(pos)) 'P' else 'p'
    } else {
      if (isCoveredByUnit(pos)) 'u' else cell(pos).toChar
    }
  }
}
