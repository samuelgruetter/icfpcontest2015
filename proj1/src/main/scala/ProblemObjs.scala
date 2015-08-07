
case class Cell(
  x: Int,
  y: Int
)

case class Unitt(
  members: List[Cell],
  pivot: Cell
)

case class Problem(
  id: Int,
  units: List[Unitt],
  width: Int,
  height: Int,
  filled: List[Cell],
  sourceLength: Int,
  sourceSeeds: List[Int]
)
