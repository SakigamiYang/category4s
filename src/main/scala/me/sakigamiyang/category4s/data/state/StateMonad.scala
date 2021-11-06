package me.sakigamiyang.category4s.data.state

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.monad.Monad

trait StateMonad[S] extends Monad[K1[State.T, S]] with StateApplicative[S] with StateFunctor[S] {
  override def join[A](v: K1[K1[State.T, S], K1[K1[State.T, S], A]]): State[S, A] = {
    val state = State.narrow(v)
    new State(s => {
      val (value, s2) = state.run(s)
      State.narrow(value).run(s2)
    })
  }

  override def bind[A, B](f: A => K1[K1[State.T, S], B], v: K1[K1[State.T, S], A]): State[S, B] =
    State.narrow(super.bind(f, v))

}
