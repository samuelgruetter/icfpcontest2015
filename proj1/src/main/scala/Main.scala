
object Main {
  def main(args: Array[String]): Unit = {
    println("hello")

    val problem = JsonRead.problemFromFile("/home/aero/dev/icfp15/probs/problem_0.json")
    println(problem)
  }
}
