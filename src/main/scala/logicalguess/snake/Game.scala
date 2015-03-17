package logicalguess.snake

import swing._
import java.awt.event.{ActionEvent, ActionListener}
import event.Key._
import akka.actor.{ActorSystem, Props, ActorRef, Actor}
import swing.Dialog._
import logicalguess.snake.GraphicConverters._
import scala.swing.event.KeyPressed


  case class ShowMessage(text: String)
  case class ReceivedPressed(keyCode: Value)

  class BoardDriver(model: ActorRef) extends Actor {
    import World._

    val directions = Map[Value, WorldLocation](
      Left -> Direction.Left,
      Right -> Direction.Right,
      Up -> Direction.Up,
      Down -> Direction.Down
    )

    def receive = {
      case ReceivedPressed(key) =>
        model ! UpdateDirection(directions(key))
    }
  }

class Board(handle: => (Value) => Unit ) extends Panel {
  var doPaint: ((Graphics2D) => Unit) = (onGraphics) => {}
  preferredSize = new Dimension(GraphicConverters.converted(World.width), GraphicConverters.converted(World.height))
  focusable = true

  override def paintComponent(onGraphic: Graphics2D) {
    super.paintComponent(onGraphic)
    doPaint(onGraphic)
  }

  listenTo(keys)

  reactions += {
    case KeyPressed(source, key, modifiers, location) =>
      handle(key)
  }

  def update(snake: List[ScreenLocation], apple: ScreenLocation) {
    def paintPoint(screenLocation: ScreenLocation, color: Color, onGraphics: Graphics2D) {
      onGraphics.setColor(color)
      onGraphics.fillRect(screenLocation.x, screenLocation.y, screenLocation.width, screenLocation.height)
    }

    doPaint = (onGraphics: Graphics2D) => {
      paintPoint(apple, new Color(210, 50, 90), onGraphics)
      snake.foreach {
        paintPoint(_, new Color(15, 160, 70), onGraphics)
      }
    }
    repaint()
  }
}

class GameC(model: ActorRef, val view: Board) extends SimpleSwingApplication {

  def displayMessage(text: String) {
    showMessage(parent = view, message = text)
  }

  def update(snake: List[WorldLocation], apple: WorldLocation) {
     view.update(converted(snake), converted(apple))
  }

  def top = new MainFrame {
    //title = "Snake"
    contents = new FlowPanel() {
      val timer = new javax.swing.Timer(150, new ActionListener() {
        def actionPerformed(e: ActionEvent) {
          model ! Refresh()
        }
      }).start()
      contents += view
    }
    pack()
  }
}

class MVC(stateClass: Class[_]) extends  {
  val system: ActorSystem = akka.actor.ActorSystem.create()
  val model = system.actorOf(Props(stateClass))
  val controller = system.actorOf(Props(classOf[BoardDriver], model))
  val view = new Board((key: Value) => controller ! ReceivedPressed(key))
}

//change the class to one of GameStateVars, GameStateRX or GameStateMonad
object GameMVC extends MVC(classOf[GameStateRX])

object Game extends GameC(GameMVC.model, GameMVC.view)


