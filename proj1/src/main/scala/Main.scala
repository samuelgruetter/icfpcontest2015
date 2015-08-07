import java.io.PrintStream
import scala.collection.mutable.ArrayBuffer


class HexGrid(val width: Int, val height: Int) {
  private val grid: ArrayBuffer[ArrayBuffer[GridCell]] =
    ArrayBuffer.fill(width)(ArrayBuffer.fill(height)(EmptyCell))

  def cell(x: Int, y: Int): GridCell = grid(x)(y)
  def setCell(x: Int, y: Int, c: GridCell): Unit = {
    grid(x)(y) = c
  }

  def printTo(ps: PrintStream): Unit = {
    for (y <- 0 until height) {
      if (y % 2 == 1) ps.print(" ")
      for (x <- 0 until width) {
        ps.print(cell(x,y).toChar)
        ps.print(" ")
      }
      ps.println()
    }
  }
}

sealed abstract class GridCell {
  def toChar: Char
}

case object EmptyCell extends GridCell {
  def toChar = '.'
}
case class UnitCell(unitt: Unit) extends GridCell {
  def toChar = '0'
}

object Main {
  def main(args: Array[String]): Unit = {
    println("hello world")

    val g = new HexGrid(5, 10)
    val u = UnitCell(())

    g.setCell(2, 3, u)
    g.setCell(2, 4, u)
    g.setCell(2, 5, u)
    g.setCell(3, 5, u)

    g.printTo(System.out)

    val problem = JsonRead.problemFromFile("/home/aero/dev/icfp15/probs/problem_0.json")
    println(problem)
  }
}
