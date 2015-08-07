import java.io.PrintStream
import scala.collection.mutable.ArrayBuffer


class HexGrid(val width: Int, val height: Int) {
  private val grid: ArrayBuffer[ArrayBuffer[Char]] = ArrayBuffer.fill(width)(ArrayBuffer.fill(height)('.'))

  def cell(x: Int, y: Int): Char = grid(x)(y)
  def setCell(x: Int, y: Int, c: Char): Unit = {
    grid(x)(y) = c
  }

  def printTo(ps: PrintStream): Unit = {
    for (y <- 0 until height) {
      if (y % 2 == 1) ps.print(" ")
      for (x <- 0 until width) {
        ps.print(cell(x,y))
        ps.print(" ")
      }
      ps.println()
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    println("hello world")

    val g = new HexGrid(5, 10)

    g.setCell(2, 3, '1')
    g.setCell(2, 4, '2')
    g.setCell(2, 5, '3')
    g.setCell(3, 5, '4')

    g.printTo(System.out)

    val problem = JsonRead.problemFromFile("/home/aero/dev/icfp15/probs/problem_0.json")
    println(problem)
  }
}
