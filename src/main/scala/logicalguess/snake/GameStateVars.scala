package logicalguess.snake

import logicalguess.snake.GraphicConverters._
import logicalguess.snake.World._
import akka.actor.{ActorSystem, Props, ActorRef, Actor}

sealed trait StateMessage

case class Refresh() extends StateMessage

case class UpdateDirection(to: WorldLocation) extends StateMessage

case class Updated(snake: List[WorldLocation], apple: WorldLocation) extends StateMessage


class GameStateVars extends Actor {

  sealed trait Entity

  case class Snake(body: List[WorldLocation], direction: WorldLocation) extends Entity {
    def go(toDirection: WorldLocation): Snake = Snake(body, toDirection)
    def moved: Snake = Snake(ahead :: body.take(body.size - 1), direction)
    def grown: Snake = Snake(ahead :: body, direction)
    def ahead: WorldLocation = head + direction
    def head: WorldLocation = body.head
  }

  case class Apple(location: WorldLocation) extends Entity


  var snake: Snake = _
  var apple: Apple = _
  reset()

  def reset() {
    snake = Snake(List(origin), Direction.Right)
    apple = Apple(randomLocation())
  }

  def updatePositions(fromSnake: Snake, fromApple: Apple) {
    fromSnake.body match {
      case head :: tail if head == fromApple.location =>
        apple = Apple(randomLocation())
        snake = fromSnake.grown
      case head :: tail if tail.contains(head) =>
        Game.displayMessage("You lose")
        reset()
      case head :: tail if tail.size == World.winLength =>
        Game.displayMessage("You win")
        reset()
      case _ => snake = fromSnake.moved
    }
    Game.view.update(converted(snake.body), converted(apple.location))
  }

  def updateDirectionOf(withSnake: Snake, to: WorldLocation) {
    snake = withSnake.go(to)
  }

  def receive = {
    case Refresh() => updatePositions(snake, apple)
    case UpdateDirection(to) => updateDirectionOf(snake, to)
  }
}

