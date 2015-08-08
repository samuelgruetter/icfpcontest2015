
import scala.collection.mutable.ListBuffer
import Movers._

object Main2 {

  def main(args: Array[String]): Unit = {
    val solutions = playAllProblems
    println(JsonWrite.solutionsToJsonString(solutions))
  }

  def playAllProblems: Seq[Solution] = {
    (0 to 23).flatMap(playOneProblem(_, randomlyDown))
  }

  def playOneProblem(problemId: Int, mover: Mover): Seq[Solution] = {
    val problem = JsonRead.problemFromFile(s"../probs/problem_$problemId.json")
    for (seed <- problem.sourceSeeds) yield playOneSeed(problem, seed, mover)
  }

  def playOneSeed(problem: Problem, seed: Int, mover: Mover): Solution = {
    val grid = HexGrid(problem)

    val units = RandomStream(seed).take(problem.sourceLength)
      .map(rand => problem.units(rand % problem.units.length))

    val commands = ListBuffer[Move]()
    var stuck = false
    var remainingUnits = units
    while (!stuck && remainingUnits.nonEmpty) {
      playOneUnit(grid, remainingUnits.head, mover) match {
        case Some(moreCommands) =>
          commands ++= moreCommands
          remainingUnits = remainingUnits.tail
        case None => stuck = true
      }
    }

    SolutionCompanion(problem.id, seed, commands)
  }

  def playOneUnit(grid: HexGrid, unit: Unitt, mover: Mover): Option[Seq[Move]] = {
    grid.spawnUnit(unit)
    if (grid.canPlaceCurrentUnitAt(grid.unitCenter)) {
      Some(mover(grid))
    } else {
      None
    }
  }

}
