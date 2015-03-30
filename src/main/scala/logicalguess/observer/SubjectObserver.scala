package logicalguess.observer

/**
 * http://programming-scala.labs.oreilly.com/ch13.html
 */
abstract class SubjectObserver {
  type S <: Subject
  type O <: Observer

  trait Subject {
    // self-type annotation
    // we can now use "self" as an alias for "this"
    self: S =>
    private var observers = List[O]()

    def addObserver(observer: O) = observers ::= observer

    def notifyObservers = observers foreach (_.onUpdate(self))
  }

  trait Observer {
    def onUpdate(subject: S)
  }
}

