package me.sakigamiyang.category4s.data.state

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.Functor

trait StateFunctor[S] extends Functor[K1[State.T, S]] {
  override def fmap[A, B](f: A => B, v: K1[K1[State.T, S], A]): State[S, B] =
    new State(s => {
      val (value, s2) = State.narrow(v).run(s)
      (f(value), s2)
    })

}
