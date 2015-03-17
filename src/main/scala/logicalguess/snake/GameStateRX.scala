package logicalguess.snake

import logicalguess.snake.GraphicConverters._
import logicalguess.snake.World._
import akka.actor.Actor
import rx.lang.scala.subjects.PublishSubject
import rx.lang.scala.{Subject, Observable}
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit

class GameStateRX extends Actor {

  sealed trait Event
  case class Turn(direction: WorldLocation) extends Event
  case class Grow() extends Event
  case class Move() extends Event

  val events: PublishSubject[Event] = PublishSubject[Event]()

  val snakeObservable = createSnake(Snake(List(origin), Direction.Right), events)

  val appleObservable = createApple(randomLocation(), snakeObservable, events)

  snakeObservable.combineLatest(appleObservable).subscribe(
    pair => {
      Game.view.update(converted(pair._1.body), converted(pair._2))
    },
    (t: Throwable) =>  t.printStackTrace(),
    () => {}
  )

  val tick = Observable.interval(Duration(150, TimeUnit.MILLISECONDS))

  tick.subscribe(
    _ => events.onNext(Move()),
    (t: Throwable) => println("tick error : " + t),
    () => {}
  )


  def createSnake(init: Snake, events: Observable[Event]): Observable[Snake] = {
    events
      .scan(init)((snake, event) => event match {
        case Move() => snake.moved
        case Grow() => snake.grown
        case Turn(direction) => snake.go(direction)
      })
  }

  def createApple(init: WorldLocation, snakeObservable: Observable[Snake], events: Subject[Event, Event]): Observable[WorldLocation] = {
    snakeObservable.scan(randomLocation())((loc: WorldLocation, snake: Snake) => snake.head match {
      case head if head == loc =>
        events.onNext(Grow())
        randomLocation()
      case _ => loc
    })
  }

  case class Snake(body: List[WorldLocation], direction: WorldLocation) {
    def go(toDirection: WorldLocation): Snake = Snake(body, toDirection)
    def moved: Snake = Snake((head + direction) :: body.take(body.size - 1), direction)
    def grown: Snake = Snake((head + direction) :: body, direction)
    def head: WorldLocation = body.head
  }

  def receive = {
    //case Refresh() => events.onNext(Move())
    case UpdateDirection(to) => events.onNext(Turn(to))
  }
}
