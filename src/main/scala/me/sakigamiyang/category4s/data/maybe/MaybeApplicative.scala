package me.sakigamiyang.category4s.data.maybe

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.applicative.Applicative

trait MaybeApplicative extends Applicative[Maybe.T] with MaybeFunctor {
  override def pure[A](v: A): Maybe[A] = Maybe.Just(v)

  override def ap[A, B](f: K1[Maybe.T, A => B], v: K1[Maybe.T, A]): Maybe[B] = {
    val maybe = Maybe.narrow(v)
    val maybef = Maybe.narrow(f)
    maybe match {
      case Maybe.Just(_) => maybef match {
        case Maybe.Just(value2) => Maybe.narrow(fmap(value2, maybe))
        case Maybe.None() => Maybe.None()
      }
      case Maybe.None() => Maybe.None()
    }
  }
}
