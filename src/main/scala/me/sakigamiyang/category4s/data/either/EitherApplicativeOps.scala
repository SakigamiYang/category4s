package me.sakigamiyang.category4s.data.either

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.applicative.ApplicativeOps

trait EitherApplicativeOps[S] extends ApplicativeOps[K1[Either.T, S]] with EitherMonad[S] {
  override def liftA[A, B](f: A => B): K1[K1[Either.T, S], A] => Either[S, B] =
    a => Either.narrow(a) ap Either.pure(f)

  override def liftA2[A, B, C](f: (A, B) => C): (K1[K1[Either.T, S], A], K1[K1[Either.T, S], B]) => Either[S, C] =
    (a1, a2) =>
      Either.narrow(a2) ap
        (Either.narrow(a1) fmap f.curried)

  override def liftA3[A, B, C, D](f: (A, B, C) => D): (K1[K1[Either.T, S], A], K1[K1[Either.T, S], B], K1[K1[Either.T, S], C]) => Either[S, D] =
    (a1, a2, a3) =>
      Either.narrow(a3) ap
        (Either.narrow(a2) ap
          (Either.narrow(a1) fmap f.curried))
}
