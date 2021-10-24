package me.sakigamiyang.category4s.data.either

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.applicative.Applicative

trait EitherApplicative[S] extends Applicative[K1[Either.T, S]] {
  override def pure[A](v: A): Either[S, A] = Either.Right(v)

  override def ap[A, B](f: K1[K1[Either.T, S], A => B], v: K1[K1[Either.T, S], A]): Either[S, B] =
    Either.narrow(f) match {
      case Either.Right(value) => Either.narrow(fmap(value, Either.narrow(v)))
      case Either.Left(value) => Either.Left(value)
    }
}
