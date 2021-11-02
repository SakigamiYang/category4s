package me.sakigamiyang.category4s.data.maybe

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.monad.MonadOps

trait MaybeMonadOps extends MonadOps[Maybe.T] with MaybeMonad {
  override def liftM[A, B](f: A => B): K1[Maybe.T, A] => K1[Maybe.T, B] =
    m => {
      val i = Maybe.narrow(m)
      i bind { x => Maybe.pure(f(x)) }
    }

  override def liftM2[A, B, C](f: (A, B) => C): (K1[Maybe.T, A], K1[Maybe.T, B]) => K1[Maybe.T, C] =
    (m1, m2) => {
      val i1 = Maybe.narrow(m1)
      val i2 = Maybe.narrow(m2)
      i1 bind { x1 => i2 bind { x2 => Maybe.pure(f(x1, x2)) } }
    }

  override def liftM3[A, B, C, D](f: (A, B, C) => D): (K1[Maybe.T, A], K1[Maybe.T, B], K1[Maybe.T, C]) => K1[Maybe.T, D] =
    (m1, m2, m3) => {
      val i1 = Maybe.narrow(m1)
      val i2 = Maybe.narrow(m2)
      val i3 = Maybe.narrow(m3)
      i1 bind { x1 => i2 bind { x2 => i3 bind { x3 => Maybe.pure(f(x1, x2, x3)) } } }
    }
}
