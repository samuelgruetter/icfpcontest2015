import spray.json._
import DefaultJsonProtocol._
import scala.io.Source

object MyJsonProtocol extends DefaultJsonProtocol {
  implicit val cellFormat = jsonFormat2(Cell)
  implicit val unitFormat = jsonFormat2(Unitt)
  implicit val problemFormat = jsonFormat7(Problem)
}
import MyJsonProtocol._

object JsonRead {
  def problemFromFile(path: String): Problem = {
    val rawContent = Source.fromFile(path).mkString
    val problem = JsonParser(rawContent).convertTo[Problem]
    problem
  }
}
