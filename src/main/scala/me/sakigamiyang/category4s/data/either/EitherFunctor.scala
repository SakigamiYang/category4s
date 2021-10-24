package me.sakigamiyang.category4s.data.either

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.Functor

trait EitherFunctor[S] extends Functor[K1[Either.T, S]] {
  override def fmap[A, B](f: A => B, v: K1[K1[Either.T, S], A]): Either[S, B] =
    Either.narrow(v) match {
      case Either.Left(value) => Either.Left(value)
      case Either.Right(value) => Either.Right(f(value))
    }
}
