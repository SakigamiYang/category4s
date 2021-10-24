package me.sakigamiyang.category4s.data.either

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.monad.Monad

trait EitherMonad[S] extends Monad[K1[Either.T, S]] with EitherApplicative[S] with EitherFunctor[S] {
  override def join[A](v: K1[K1[Either.T, S], K1[K1[Either.T, S], A]]): Either[S, A] =
    Either.narrow(v) match {
      case Either.Right(value) => Either.narrow(value)
      case Either.Left(value) => Either.Left(value)
    }

  override def bind[A, B](f: A => K1[K1[Either.T, S], B], v: K1[K1[Either.T, S], A]): Either[S, B] =
    Either.narrow(super.bind(f, v))
}
