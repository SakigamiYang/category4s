package me.sakigamiyang.category4s.data.maybe

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.Functor

trait MaybeFunctor extends Functor[Maybe.T] {
  override def fmap[A, B](f: A => B, v: K1[Maybe.T, A]): Maybe[B] =
    Maybe.narrow(v) match {
      case Maybe.Just(value) => Maybe.Just(f(value))
      case Maybe.None() => Maybe.None()
    }
}
