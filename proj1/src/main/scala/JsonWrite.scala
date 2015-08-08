import spray.json._

object MyJsonWriteProtocol extends DefaultJsonProtocol {
  implicit val solutionFormat = jsonFormat3(Solution)
}
import MyJsonWriteProtocol._

object JsonWrite {
  def solutionsToJsonString(sols: Seq[Solution]): String = {
    sols.toJson.prettyPrint
  }
}
