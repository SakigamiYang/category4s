package me.sakigamiyang.category4s.data.state

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.monad.MonadOps

trait StateMonadOps[S] extends MonadOps[K1[State.T, S]] with StateMonad[S] {
  override def liftM[A, B](f: A => B): K1[K1[State.T, S], A] => K1[K1[State.T, S], B] =
    m => {
      val i = State.narrow(m)
      i bind { x => State.pure(f(x)) }
    }

  override def liftM2[A, B, C](f: (A, B) => C): (K1[K1[State.T, S], A], K1[K1[State.T, S], B]) => K1[K1[State.T, S], C] =
    (m1, m2) => {
      val i1 = State.narrow(m1)
      val i2 = State.narrow(m2)
      i1 bind { x1 => i2 bind { x2 => State.pure(f(x1, x2)) } }
    }

  override def liftM3[A, B, C, D](f: (A, B, C) => D): (K1[K1[State.T, S], A], K1[K1[State.T, S], B], K1[K1[State.T, S], C]) => K1[K1[State.T, S], D] =
    (m1, m2, m3) => {
      val i1 = State.narrow(m1)
      val i2 = State.narrow(m2)
      val i3 = State.narrow(m3)
      i1 bind { x1 => i2 bind { x2 => i3 bind { x3 => State.pure(f(x1, x2, x3)) } } }
    }

}
