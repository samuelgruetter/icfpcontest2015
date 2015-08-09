import java.io.PrintStream
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import java.lang.IndexOutOfBoundsException


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

  /*
  val clearedLinesStats: mutable.Map[Int, Int] = mutable.Map()
  def clearedLinesHook(nClearedLines: Int): Unit = {
    val old = clearedLinesStats.getOrElse(nClearedLines, 0)
    clearedLinesStats.put(nClearedLines, old+1)
  }
  */
}

class HexGrid(val width: Int, val height: Int) {
  // in offset coordinates
  private val grid: ArrayBuffer[ArrayBuffer[GridCell]] =
    ArrayBuffer.fill(width)(ArrayBuffer.fill(height)(EmptyCell))

  // get and set cells in axial coordinates
  def cell(pos: Cell): GridCell = {
    val offset = pos.toOffset
    try {
      grid(offset.x)(offset.y)
    } catch {
      case _: IndexOutOfBoundsException => FullCell
    }
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


  def isReallyEmpty(xOffset: Int, yOffset: Int): Boolean = {
    grid(xOffset)(yOffset) == EmptyCell && !isCoveredByUnit(Cell(xOffset, yOffset).toAxial)
  }

  def canPlaceCurrentUnitAt(p0: Cell): Boolean = {
    unit.members.forall(c => isCellEmpty(p0.add(c)))
  }

  //def row(y: Int): Iterable[GridCell] = Iterable.range(0, width).map(x => grid(x)(y))

  def countFullCellsOfRow(y: Int): Int = (0 until width).count(x => grid(x)(y) == FullCell)

  def numberOfClearedLinesIfUnitLockedAt(p: Cell): Int = {
    // val map2 = unit.members.groupBy(mem => cell(p.add(mem)))
    // if (map2.isDefinedAt(FullCell)) throw new IllegalArgumentException("cannot lock unit at " + p)

    val res = unit.members.map(mem => p.add(mem)).groupBy(_.y).count {
      case (y, newlyFilledCells) => newlyFilledCells.length + countFullCellsOfRow(y) == width
    }
    //HexGrid.clearedLinesHook(res)
    res
  }

  def countEmptyRegionsOfRow(y: Int): Int = {
    var lastFull: Boolean = true
    var count = 0
    for (x <- 0 until width) {
      val currentEmpty = isReallyEmpty(x, y)
      if (lastFull && currentEmpty) count += 1
      lastFull = !currentEmpty
    }
    count
  }

  def countEmptyRegionsIfUnitLockedAt(p: Cell): Int = {
    val savedCenter = unitCenter
    unitCenter = p
    val res = Iterable.range(0, height).map(countEmptyRegionsOfRow).sum
    unitCenter = savedCenter
    res
  }

  def canMove(dir: Cell): Boolean = {
    canPlaceCurrentUnitAt(unitCenter.add(dir))
  }

  def applyMove(move: Move): Unit = {
    move match {
      case _: Rotation => throw new UnsupportedOperationException // TODO
      case s: Step => if (s.locks) lockUnit() else step(s.direction)
    }
  }

  def step(dir: Cell): Unit = {
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
    removeFullLines()
  }

  def removeFullLines(): Unit = {
    for (y <- 0 until height) {
      if ((0 until width).forall(x => grid(x)(y) == FullCell)) {
        for (y2 <- y to 0 by -1) {
          for (x2 <- 0 until width) {
            grid(x2)(y2) = if (y2 == 0) EmptyCell else grid(x2)(y2 - 1)
          }
        }
      }
    }
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

  // assumes axial coordinates
  def cellToChar(pos: Cell): Char = {
    if (pos == currentUnitPivot) {
      if (isCoveredByUnit(pos)) 'P' else 'p'
    } else {
      if (isCoveredByUnit(pos)) 'u' else cell(pos).toChar
    }
  }
}
