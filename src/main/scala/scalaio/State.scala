package scalaio

/**
 * @author David Galichet.
 */
trait State[S, +A] {

    def run(initial: S): (S, A)

    def map[B](f: A => B): State[S, B] = State { s =>
        val (s1, a) = run(s)
        (s1, f(a))
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
        val (s1, a) = run(s)
        f(a).run(s1)
    }
}

object State {
    def apply[S, A](f: S => (S, A)): State[S, A] = new State[S, A] {
        def run(initial: S): (S, A) = f(initial)

    }

    def get[S]: State[S, S] = State { s => (s, s) }

    def gets[S, A](f: S => A): State[S, A] = State { s => (s, f(s)) }
}
