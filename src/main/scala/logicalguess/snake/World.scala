package logicalguess.snake

import scala.util.Random._

case class WorldLocation(x: Int, y:Int) {
  def + (location: WorldLocation) : WorldLocation ={
    WorldLocation(x + location.x, y + location.y)
  }
}

object World {
  def winLength = 5

  object Direction {
    val Left = WorldLocation(-1, 0)
    val Right = WorldLocation(1, 0)
    val Down = WorldLocation(0, 1)
    val Up = WorldLocation(0, -1)
  }

  def origin: WorldLocation = WorldLocation(0,0)

  def randomLocation(): WorldLocation = WorldLocation(nextInt(width), nextInt(height))

  def height: Int = 25
  def width: Int = 25

}