import java.io.PrintStream

import scala.collection.mutable.ArrayBuffer
import scala.math.{min, max}

case class Cell(
  x: Int,
  y: Int
)

case class Unitt(
  members: List[Cell],
  pivot: Cell
) {
  def leftmost: Int = members.minBy(_.x).x
  def rightmost: Int = members.maxBy(_.x).x
  def topmost: Int = members.minBy(_.y).y
  def bottommost: Int = members.maxBy(_.y).y

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

case class Problem(
  id: Int,
  units: List[Unitt],
  width: Int,
  height: Int,
  filled: List[Cell],
  sourceLength: Int,
  sourceSeeds: List[Int]
)
