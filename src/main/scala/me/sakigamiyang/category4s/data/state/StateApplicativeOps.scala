package me.sakigamiyang.category4s.data.state

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.applicative.ApplicativeOps

trait StateApplicativeOps[S] extends ApplicativeOps[K1[State.T, S]] with StateMonad[S] {
  override def liftA[A, B](f: A => B): K1[K1[State.T, S], A] => State[S, B] =
    a => State.narrow(a) ap State.pure(f)

  override def liftA2[A, B, C](f: (A, B) => C): (K1[K1[State.T, S], A], K1[K1[State.T, S], B]) => State[S, C] =
    (a1, a2) => {
      val i1 = State.narrow(a1)
      val i2 = State.narrow(a2)
      i2 ap (i1 fmap f.curried)
    }

  override def liftA3[A, B, C, D](f: (A, B, C) => D): (K1[K1[State.T, S], A], K1[K1[State.T, S], B], K1[K1[State.T, S], C]) => State[S, D] =
    (a1, a2, a3) => {
      val i1 = State.narrow(a1)
      val i2 = State.narrow(a2)
      val i3 = State.narrow(a3)
      i3 ap (i2 ap (i1 fmap f.curried))
    }
}
