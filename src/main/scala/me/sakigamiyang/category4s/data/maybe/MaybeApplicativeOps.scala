package me.sakigamiyang.category4s.data.maybe

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.applicative.ApplicativeOps

trait MaybeApplicativeOps extends ApplicativeOps[Maybe.T] with MaybeApplicative {
  override def liftA[A, B](f: A => B): K1[Maybe.T, A] => Maybe[B] =
    a => Maybe.narrow(a) ap Maybe.pure()

  override def liftA2[A, B, C](f: (A, B) => C): (K1[Maybe.T, A], K1[Maybe.T, B]) => Maybe[C] =
    (a1, a2) => {
      val i1 = Maybe.narrow(a1)
      val i2 = Maybe.narrow(a2)
      i2 ap {
        i1 fmap f.curried
      }
    }

  override def liftA3[A, B, C, D](f: (A, B, C) => D): (K1[Maybe.T, A], K1[Maybe.T, B], K1[Maybe.T, C]) => Maybe[D] =
    (a1, a2, a3) => {
      val i1 = Maybe.narrow(a1)
      val i2 = Maybe.narrow(a2)
      val i3 = Maybe.narrow(a3)
      i3 ap {
        i2 ap {
          i1 fmap f.curried
        }
      }
    }
}
