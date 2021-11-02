package me.sakigamiyang.category4s.data.maybe

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.monad.Monad

trait MaybeMonad extends Monad[Maybe.T] with MaybeApplicative with MaybeFunctor {
  override def join[A](v: K1[Maybe.T, K1[Maybe.T, A]]): Maybe[A] = {
    val maybe = Maybe.narrow(v)
    maybe match {
      case Maybe.Just(value) => Maybe.narrow(value)
      case Maybe.None() => Maybe.None()
    }
  }

  override def bind[A, B](f: A => K1[Maybe.T, B], v: K1[Maybe.T, A]): Maybe[B] =
    Maybe.narrow(super.bind(f, v))
}
