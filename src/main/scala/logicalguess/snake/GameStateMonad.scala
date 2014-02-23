package logicalguess.snake

import logicalguess.snake.World._
import akka.actor.{ActorRef, Actor}
import scalaio.State


class GameStateMonad(listener: ActorRef) extends Actor {

  var playground: Playground = _
  var state: State[Playground, Score] = _

  def reset(snakeOpt: Option[Snake]) {
    playground = Playground(
      WorldLocation(0, 0),
      WorldLocation(World.width - 1, World.heigth - 1),
      randomLocation(),
      snakeOpt.getOrElse(Snake(Position(15, 15, North) :: Nil)))

    state = State[Playground, Score] {s =>
        (s, s.snake.score)
    }
  }

  reset(None)

  def receive = monad

  def process: Receive = {
    case Refresh() => {
      playground = processDirection(Unknown)(playground)
      notifyListener(playground)
    }
    case UpdateDirection(to) => {
      playground = processDirection(toDirection(to))(playground)
      notifyListener(playground)
    }
  }

  def monad: Receive = {
    case Refresh() => {
      state = state.flatMap(_ => compileDirection(Unknown))
      notifyListener(state.run(playground)._1)

    }
    case UpdateDirection(to) => {
      state = state.flatMap(_ => compileDirection(toDirection(to)))
      notifyListener(state.run(playground)._1)
    }
  }

  def compile: Receive = {
    case Refresh() => {
      playground = compileDirection(Unknown).run(playground)._1
      notifyListener(playground)
    }
    case UpdateDirection(to) => {
      playground = compileDirection(toDirection(to)).run(playground)._1
      notifyListener(playground)
    }
  }




  def notifyListener(s: Playground): Unit = {
    listener ! Updated(s.snake.positions.map(_.point), s.apple)
  }

  def processDirection(d: Direction)(s: Playground): Playground = {
    val pos = s.snake.currentPosition
    val next = d match {
      case d if (d == Unknown || d == pos.direction) => {
        pos.move(s)
      }
      case d => pos.turn(d)
    }

    if (s.apple.equals(next.point)) {
      var snake = s.snake.addApple(next.point).addPosition(toPosition(next.point, s.snake.currentPosition.direction), 0)
      reset(Some(snake))
      s.copy(apple = randomLocation(), snake = snake)
    } else {
      s.copy(snake = s.snake.addPosition(next, 1))
    }
  }

  def compileDirection(direction: Direction): State[Playground, Score] = {
    State[Playground, Score] { s =>
      val s1 = processDirection(direction)(s)
      (s1, s1.snake.score)
    }
  }

  def toDirection(location: WorldLocation): Direction = location match {
    case WorldLocation(-1, 0) => West
    case WorldLocation(1, 0) => East
    case WorldLocation(0, 1) => North
    case WorldLocation(0, -1) => South
    case _ => Unknown
  }

  def toPosition(location: WorldLocation, direction: Direction): Position  = {
    Position(location, direction)
  }
}

case class Playground(bottomLeft: WorldLocation, topRight: WorldLocation, apple: WorldLocation, snake: Snake) {

  assert(bottomLeft.x < topRight.x && bottomLeft.y < topRight.y,
    s"Bad Playground definition. (${bottomLeft.x}, ${bottomLeft.y}) must be < (${topRight.x}, ${topRight.y})")
  assert(isInPlayground(apple), s"The apple must be in Playground ! - $apple")

  def isInPlayground(point: WorldLocation): Boolean =
    bottomLeft.x <= point.x && point.x <= topRight.x && bottomLeft.y <= point.y && point.y <= topRight.y

  def isPossiblePosition(pos: Position): Boolean = isInPlayground(pos.point)

  lazy val score: Score = snake.score

}

case class Snake(positions: List[Position], apples: List[WorldLocation] = Nil) {
  lazy val currentPosition = positions.head
  lazy val currentLocation = positions.head.point
  lazy val score: Score = Score(apples.size)

  lazy val location: WorldLocation = positions.head.point

  def addPosition(next: Position, dropCount: Int): Snake = this.copy(positions = next::positions.dropRight(dropCount))

  def addApple(apple: WorldLocation): Snake = this.copy(apples = apple::apples)
}

case class Score(score: Int)

case class Position(point: WorldLocation, direction: Direction) {
  def move(s: Playground): Position = {
    val newPosition = this.direction match { // Good candidate for Lenses !
      case North => this.copy(point = this.point.copy(y = this.point.y + 1))
      case South => this.copy(point = this.point.copy(y = this.point.y - 1))
      case East => this.copy(point = this.point.copy(x = this.point.x + 1))
      case West => this.copy(point = this.point.copy(x = this.point.x - 1))
      case _ => this
    }
    if (s.isPossiblePosition(newPosition)) newPosition else this
  }

  def turn(d: Direction): Position = this.copy(direction = d)
}

object Position {
  def apply(x: Int, y: Int, direction: Direction): Position = Position(WorldLocation(x, y), direction)
}

sealed trait Direction

case object Unknown extends Direction
case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction





