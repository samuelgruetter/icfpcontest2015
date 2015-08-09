import java.io.PrintStream
import java.util.Scanner
import scala.util.Random


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

  def printOneProblem(problem: Problem, ps: PrintStream): Unit = {
    ps.println(s"Problem ${problem.id} (${problem.width} x ${problem.height}):")
    ps.println(s"Source length: ${problem.sourceLength}")
    ps.println(s"Source seeds: ${problem.sourceSeeds.mkString(", ")}")
    ps.println(s"${problem.units.length} units:\n")
    for (u <- problem.units) {
      u.printMapTo(ps)
      ps.println
      u.toAxial.rotate(true).toOffset.printMapTo(ps)
      ps.println
      u.toAxial.rotate(false).toOffset.printMapTo(ps)
      ps.println
    }
    ps.println("Grid:")
    val grid = HexGrid(problem)
    grid.printTo(ps)
  }

  def printAllProbs: Unit = {
    for (i <- 0 to 24) {
      println("\n")
      println("-------------------------------------------------------------")
      val problem = JsonRead.problemFromFile(s"../probs/problem_$i.json")
      printOneProblem(problem, System.out)
    }
    println
  }

  def demoUnitPlacing(problemId: Int, unitId: Int): Unit = {
    val problem = JsonRead.problemFromFile(s"../probs/problem_$problemId.json")
    val grid = HexGrid(problem)

    grid.spawnUnit(problem.units(unitId))

    grid.printTo(System.out)
    println
  }

  def randomlyDown(problemId: Int, unitId: Int): Unit = {
    val problem = JsonRead.problemFromFile(s"../probs/problem_$problemId.json")
    val grid = HexGrid(problem)

    grid.spawnUnit(problem.units(unitId))

    val rd = new Scanner(System.in)
    val rand = new Random(123456789)
    var locked = false
    while (!locked) {
      grid.printTo(System.out)
      println
      rd.nextLine()

      val moves = if (rand.nextBoolean()) List(Directions.southWest, Directions.southEast) else List(Directions.southEast, Directions.southWest)
      val possibleMoves = moves.filter(grid.canMove)
      if (possibleMoves.isEmpty) {
        println("locked")
        locked = true
      } else {
        grid.step(possibleMoves.head)
      }
    }
  }

  def main1(args: Array[String]): Unit = {
    printAllProbs
    demoUnitPlacing(18, 25)
    demoUnitPlacing(16, 4)
  }

  def main(args: Array[String]): Unit = {
    main1(args)
    randomlyDown(16, 4)
    //println(Cell(1, 0).rotate(Cell(0, 0), true))
  }
}
