package logicalguess.observer

trait Clickable {
  def click()
}

class Button(val label: String) extends Clickable {
  def click() = println("clicked on " + label)
}

object ButtonContext extends SubjectObserver {
  type S = ObservableButton
  type O = ButtonObserver

  class ObservableButton(name: String) extends Button(name) with Subject {
    override def click() = {
      super.click
      notifyObservers
    }
  }

  trait ButtonObserver extends Observer {
    def onUpdate(button: ObservableButton)
  }

  import scala.collection.mutable.HashMap

  class ButtonClickObserver extends ButtonContext.ButtonObserver {
    val clicks = new HashMap[String, Int]

    def onUpdate(button: ButtonContext.ObservableButton) = {
      val count = clicks.getOrElse(button.label, 0) + 1
      clicks.update(button.label, count)
    }
  }

}

object Main extends App {

  val button1 = new ButtonContext.ObservableButton("button1")
  val button2 = new ButtonContext.ObservableButton("button2")
  val button3 = new ButtonContext.ObservableButton("button3")

  val observer = new ButtonContext.ButtonClickObserver

  button1.addObserver(observer)
  button2.addObserver(observer)
  button3.addObserver(observer)

  clickButton(button1, 1)
  clickButton(button2, 2)
  clickButton(button3, 3)

  assert(observer.clicks.get("button1").get == 1)
  assert(observer.clicks.get("button2").get == 2)
  assert(observer.clicks.get("button3").get == 3)

  def clickButton(button: Button, nClicks: Int) = {
    for (i <- 1 to nClicks)
      button.click()
  }

}
