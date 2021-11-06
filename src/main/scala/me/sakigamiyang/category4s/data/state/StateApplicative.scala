package me.sakigamiyang.category4s.data.state

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.applicative.Applicative

trait StateApplicative[S] extends Applicative[K1[State.T, S]] {
  override def pure[A](v: A): State[S, A] = new State(s => (v, s))

  override def ap[A, B](f: K1[K1[State.T, S], A => B], v: K1[K1[State.T, S], A]): State[S, B] = {
    val state = State.narrow(v)
    val statef = State.narrow(f)
    new State(s => {
      val (value, s2) = statef.run(s)
      State.narrow(fmap(value, state)).run(s2)
    })
  }
}
