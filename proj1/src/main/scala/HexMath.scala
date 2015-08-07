
object HexMath {

  // functions for normal coordinates (problem statement)

  def toCube(x: Int, y: Int): (Int, Int, Int) = {
    val xp = x - math.floor(y / 2.0).toInt
    (xp, y, -xp - y)
  }

  def rotate(pointX: Int, pointY: Int, pivotX: Int, pivotY: Int, isClockWise: Boolean): (Int, Int) = {
    val (ptx, pty, ptz) = toCube(pointX, pointY)
    val (pvx, pvy, pvz) = toCube(pivotX, pivotY)
    val (sx, sy, sz) = (ptx - pvx, pty - pvy, ptz - pvz) // shift so pivot is in the center
    val (rx, ry, rz) = rotateCenter(sx, sy, sz, isClockWise)
    fromCube(rx + pvx, ry + pvy, rz + pvz)
  }

  // functions for cube coordinates (http://www.redblobgames.com/grids/hexagons/)

  def fromCube(x: Int, y: Int, z: Int): (Int, Int) = {
    (x + math.floor(y / 2.0).toInt, y)
  }

  def rotateCenter(x: Int, y: Int, z: Int, isClockWise: Boolean): (Int, Int, Int) = {
    if (isClockWise) (-y, -z, -x)
    else             (-z, -x, -y)
  }
}
