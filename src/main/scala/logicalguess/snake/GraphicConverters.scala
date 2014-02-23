package logicalguess.snake

case class ScreenLocation(x: Int, y:Int, width: Int, height: Int)


object GraphicConverters {
  val factor = 10

  def converted(length: Int): Int = length * factor

  def converted(location: WorldLocation): ScreenLocation = diplayPointFrom(location)

  def converted(segment: List[WorldLocation]): List[ScreenLocation] = segment.map(diplayPointFrom(_))


  def diplayPointFrom(location: WorldLocation): ScreenLocation = {
    ScreenLocation(location.x * factor, location.y * factor, factor, factor)
  }

}