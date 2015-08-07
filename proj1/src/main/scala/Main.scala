import java.io.PrintStream
import scala.collection.mutable.ArrayBuffer


object HexGrid {
  def apply(prob: Problem): HexGrid = {
    val g = new HexGrid(prob.width, prob.height)
    for (Cell(x, y) <- prob.filled) g.setCell(x, y, FullCell)
    g
  }
}

class HexGrid(val width: Int, val height: Int) {
  private val grid: ArrayBuffer[ArrayBuffer[GridCell]] =
    ArrayBuffer.fill(width)(ArrayBuffer.fill(height)(EmptyCell))

  var currentUnitCenter: Option[(Int, Int)] = None

  var currentUnit: Option[Unitt] = None

  def currentUnitPivot: (Int, Int) =
    (currentUnitCenter.get._1 + currentUnit.get.pivot.x, currentUnitCenter.get._2 + currentUnit.get.pivot.y)

  /** places u on the grid such that u's local (0,0) is at (x0,y0)
  def placeUnitAt(u: Unitt, x0: Int, y0: Int): Unit = {
    for (Cell(x, y) <- u.members) setCell(x0 + x, y0 + y, UnitCell)
  }*/

  def placeUnit(u: Unitt): (Int, Int) = {
    val xLeftRel = u.leftmost
    val xRightRel = u.rightmost
    val w = xRightRel - xLeftRel + 1
    val xLeftAbs = (width - w) / 2
    val xCenterAbs = xLeftAbs - xLeftRel

    val yCenterAbs = -u.topmost

    (xCenterAbs, yCenterAbs)
  }

  def cell(x: Int, y: Int): GridCell = grid(x)(y)
  def setCell(x: Int, y: Int, c: GridCell): Unit = {
    grid(x)(y) = c
  }

  def printTo(ps: PrintStream): Unit = {
    for (y <- 0 until height) {
      if (y % 2 == 1) ps.print(" ")
      for (x <- 0 until width) {
        ps.print(cellToChar(x, y))
        ps.print(" ")
      }
      ps.println()
    }
  }

  def isCoveredByUnit(x: Int, y: Int): Boolean = currentUnit match {
    // members are in relative coordinates
    case Some(cu) => cu.members.contains(Cell(x-currentUnitCenter.get._1, y-currentUnitCenter.get._2))
    case None => false
  }

  def isCurrentUnitPivot(x: Int, y: Int): Boolean = currentUnitCenter match {
    case Some(cuc) => (x, y) == currentUnitPivot
    case None => false
  }

  def cellToChar(x: Int, y: Int): Char = {
    if (isCurrentUnitPivot(x, y)) {
      if (isCoveredByUnit(x, y)) 'P' else 'p'
    } else {
      if (isCoveredByUnit(x, y)) 'u' else cell(x, y).toChar
    }
  }

  def allNeighbors(x: Int, y: Int): Iterable[(Int, Int)] = {
    Iterable() // TODO
  }
}

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

object Main {
  /*
  def main0(args: Array[String]): Unit = {
    println("hello world")

    val g = new HexGrid(5, 10)
    val u = UnitCell

    g.setCell(2, 3, u)
    g.setCell(2, 4, u)
    g.setCell(2, 5, u)
    g.setCell(3, 5, u)
    g.setCell(3, 6, FullCell)

    g.printTo(System.out)
  }
  */

  def printAllProbs: Unit = {
    for (i <- 0 to 23) {
      println("\n")
      println("-------------------------------------------------------------")
      val problem = JsonRead.problemFromFile(s"../probs/problem_$i.json")
      println(s"Problem $i (${problem.width} x ${problem.height}):")
      println(s"Source length: ${problem.sourceLength}")
      println(s"Source seeds: ${problem.sourceSeeds.mkString(", ")}")
      println(s"${problem.units.length} units:\n")
      for (u <- problem.units) {
        u.printMapTo(System.out)
        println
      }
      println("Grid:")
      val grid = HexGrid(problem)
      grid.printTo(System.out)
    }
    println
  }

  def main(args: Array[String]): Unit = {
    printAllProbs

    val problem = JsonRead.problemFromFile("../probs/problem_18.json")
    val grid = HexGrid(problem)

    grid.currentUnitCenter = Some(grid.placeUnit(problem.units(25)))
    grid.currentUnit = Some(problem.units(25))

    grid.printTo(System.out)

  }
}
