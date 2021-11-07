package me.sakigamiyang.category4s.data.reader

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.applicative.ApplicativeOps

trait ReaderApplicativeOps[R] extends ApplicativeOps[K1[Reader.T, R]] with ReaderMonad[R] {
  override def liftA[A, B](f: A => B): K1[K1[Reader.T, R], A] => Reader[R, B] =
    a =>
      Reader.narrow(a) ap Reader.pure(f)

  override def liftA2[A, B, C](f: (A, B) => C): (K1[K1[Reader.T, R], A], K1[K1[Reader.T, R], B]) => Reader[R, C] =
    (a1, a2) => {
      val i1 = Reader.narrow(a1)
      val i2 = Reader.narrow(a2)
      i2 ap {
        i1 fmap f.curried
      }
    }

  override def liftA3[A, B, C, D](f: (A, B, C) => D): (K1[K1[Reader.T, R], A], K1[K1[Reader.T, R], B], K1[K1[Reader.T, R], C]) => Reader[R, D] =
    (a1, a2, a3) => {
      val i1 = Reader.narrow(a1)
      val i2 = Reader.narrow(a2)
      val i3 = Reader.narrow(a3)
      i3 ap {
        i2 ap {
          i1 fmap f.curried
        }
      }
    }
}
