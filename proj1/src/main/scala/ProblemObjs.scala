import java.io.PrintStream

import scala.collection.mutable.ArrayBuffer
import scala.math.{min, max}

// a 2d vector
case class Cell(
  x: Int,
  y: Int
) {
  def toAxial = Cell(x + math.floor((y + 1) / 2.0).toInt, y)
  def toOffset = Cell(x - math.floor((y + 1) / 2.0).toInt, y)

  // assumes axial coordinates
  def add(other: Cell) = Cell(x + other.x, y + other.y)

  // assumes axial coordinates
  def negative = Cell(-x, -y)

  def allNeighbors(c: Cell): Iterable[Cell] = Directions.all.map(add(_))
  def forwardNeighbors(c: Cell): Iterable[Cell] = Directions.forward.map(add(_))
  def backwardNeighbors(c: Cell): Iterable[Cell] = Directions.backward.map(add(_))
  def downNeighbors(c: Cell): Iterable[Cell] = Directions.down.map(add(_))

  // assumes axial coordinates
  def rotate(pivot: Cell, isClockWise: Boolean): Cell = {
    val (dx, dy) = (x - pivot.x, y - pivot.y)
    val (rx, ry) =
      if (isClockWise) (dx - dy, dx)
      else             (dy, -dx + dy)
    Cell(rx + pivot.x, ry + pivot.y)
  }
}

case class Unitt(
  members: List[Cell],
  pivot: Cell
) {
  def toAxial = Unitt(members.map(_.toAxial), pivot.toAxial)
  def toOffset = Unitt(members.map(_.toOffset), pivot.toOffset)

  // assumes offset coordinates
  def leftmost: Int = members.minBy(_.x).x
  def rightmost: Int = members.maxBy(_.x).x
  def topmost: Int = members.minBy(_.y).y
  def bottommost: Int = members.maxBy(_.y).y

  def rotate(isClockWise: Boolean): Unitt = {
    Unitt(members.map(c => c.rotate(pivot, isClockWise)), pivot)
  }

  // assumes offset coordinates
  def printMapTo(ps: PrintStream): Unit = {
    val lm = min(leftmost, pivot.x)
    val rm = max(rightmost, pivot.x)
    val tm = min(topmost, pivot.y)
    val bm = max(bottommost, pivot.y)

    val width = rm - lm + 1
    val height = bm - tm + 1

    val buf: ArrayBuffer[ArrayBuffer[Char]] = ArrayBuffer.fill(width)(ArrayBuffer.fill(height)('.'))

    for (Cell(x, y) <- members) buf(x - lm)(y - tm) = 'u'
    if (buf(pivot.x - lm)(pivot.y - tm) == 'u') {
      buf(pivot.x - lm)(pivot.y - tm) = 'P'
    } else {
      buf(pivot.x - lm)(pivot.y - tm) = 'p'
    }

    for (y <- tm to bm) {
      if (y % 2 != 0) ps.print(" ")
      for (x <- lm to rm) {
        ps.print(buf(x - lm)(y - tm))
        ps.print(" ")
      }
      ps.println()
    }
  }
}

object Unitt {
  def empty: Unitt = Unitt(Nil, Cell(-1000000000, -1000000000))
}

case class Problem(
  id: Int,
  units: List[Unitt],
  width: Int,
  height: Int,
  filled: List[Cell],
  sourceLength: Int,
  sourceSeeds: List[Int]
) {
  def toAxial = Problem(id, units.map(_.toAxial), width, height, filled.map(_.toAxial), sourceLength, sourceSeeds)
  def toOffset = Problem(id, units.map(_.toOffset), width, height, filled.map(_.toOffset), sourceLength, sourceSeeds)
}
