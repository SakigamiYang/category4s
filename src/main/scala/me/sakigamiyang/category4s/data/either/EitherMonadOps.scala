package me.sakigamiyang.category4s.data.either

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.data.either.Either.T
import me.sakigamiyang.category4s.types.monad.MonadOps

trait EitherMonadOps[S] extends MonadOps[K1[Either.T, S]] with EitherMonad[S] {
  override def liftM[A, B](f: A => B): K1[K1[T, S], A] => K1[K1[T, S], B] =
    m => Either.narrow(m) bind { x =>
      Either.pure(f(x))
    }

  override def liftM2[A, B, C](f: (A, B) => C): (K1[K1[T, S], A], K1[K1[T, S], B]) => K1[K1[T, S], C] =
    (m1, m2) => Either.narrow(m1) bind { x1 =>
      Either.narrow(m2) bind { x2 =>
        Either.pure(f(x1, x2))
      }
    }

  override def liftM3[A, B, C, D](f: (A, B, C) => D): (K1[K1[T, S], A], K1[K1[T, S], B], K1[K1[T, S], C]) => K1[K1[T, S], D] =
    (m1, m2, m3) => Either.narrow(m1) bind { x1 =>
      Either.narrow(m2) bind { x2 =>
        Either.narrow(m3) bind { x3 =>
          Either.pure(f(x1, x2, x3))
        }
      }
    }
}
