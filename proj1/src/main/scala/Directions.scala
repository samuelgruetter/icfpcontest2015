
object Directions {
  val west = Cell(-1, 0)
  val east = Cell( 1, 0)
  val southWest = Cell( 0,  1)
  val southEast = Cell( 1,  1)
  val northWest = Cell(-1, -1)
  val northEast = Cell( 0, -1)
  
  val all = Iterable(west, east, southWest, southEast, northWest, northEast)
  val forward = Iterable(west, east, southWest, southEast)
  val backward = Iterable(west, east, northWest, northEast)
  val down = Iterable(southWest, southEast)
}