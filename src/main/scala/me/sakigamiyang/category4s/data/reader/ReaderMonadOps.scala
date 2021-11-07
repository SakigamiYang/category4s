package me.sakigamiyang.category4s.data.reader

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.monad.MonadOps

trait ReaderMonadOps[R] extends MonadOps[K1[Reader.T, R]] with ReaderMonad[R] {
  override def liftM[A, B](f: A => B): K1[K1[Reader.T, R], A] => K1[K1[Reader.T, R], B] =
    m => {
      val i = Reader.narrow(m)
      i bind { x => Reader.pure(f(x)) }
    }

  override def liftM2[A, B, C](f: (A, B) => C): (K1[K1[Reader.T, R], A], K1[K1[Reader.T, R], B]) => K1[K1[Reader.T, R], C] =
    (m1, m2) => {
      val i1 = Reader.narrow(m1)
      val i2 = Reader.narrow(m2)
      i1 bind { x1 => i2 bind { x2 => Reader.pure(f(x1, x2)) } }
    }

  override def liftM3[A, B, C, D](f: (A, B, C) => D): (K1[K1[Reader.T, R], A], K1[K1[Reader.T, R], B], K1[K1[Reader.T, R], C]) => K1[K1[Reader.T, R], D] =
    (m1, m2, m3) => {
      val i1 = Reader.narrow(m1)
      val i2 = Reader.narrow(m2)
      val i3 = Reader.narrow(m3)
      i1 bind { x1 => i2 bind { x2 => i3 bind { x3 => Reader.pure(f(x1, x2, x3)) } } }
    }
}
