package me.sakigamiyang.category4s.data.writer

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.group.Monoid
import me.sakigamiyang.category4s.types.monad.MonadOps

trait WriterMonadOps[W <: Monoid[Any, W]] extends MonadOps[K1[Writer.T, W]] with WriterMonad[W] {
  override def liftM[A, B](f: A => B): K1[K1[Writer.T, W], A] => K1[K1[Writer.T, W], B] =
    m => {
      val i = Writer.narrow(m)
      i bind { x => Writer.pure(clz, f(x)) }
    }

  override def liftM2[A, B, C](f: (A, B) => C): (K1[K1[Writer.T, W], A], K1[K1[Writer.T, W], B]) => K1[K1[Writer.T, W], C] =
    (m1, m2) => {
      val i1 = Writer.narrow(m1)
      val i2 = Writer.narrow(m2)
      i1 bind { x1 => i2 bind { x2 => Writer.pure(clz, f(x1, x2)) } }
    }

  override def liftM3[A, B, C, D](f: (A, B, C) => D): (K1[K1[Writer.T, W], A], K1[K1[Writer.T, W], B], K1[K1[Writer.T, W], C]) => K1[K1[Writer.T, W], D] =
    (m1, m2, m3) => {
      val i1 = Writer.narrow(m1)
      val i2 = Writer.narrow(m2)
      val i3 = Writer.narrow(m3)
      i1 bind { x1 => i2 bind { x2 => i3 bind { x3 => Writer.pure(clz, f(x1, x2, x3)) } } }
    }
}
