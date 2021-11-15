package me.sakigamiyang.category4s.data.writer

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.applicative.ApplicativeOps
import me.sakigamiyang.category4s.types.group.Monoid

trait WriterApplicativeOps[W <: Monoid[Any, W]] extends ApplicativeOps[K1[Writer.T, W]] with WriterMonad[W] {
  override def liftA[A, B](f: A => B): K1[K1[Writer.T, W], A] => Writer[W, B] =
    a => Writer.narrow(a) ap Writer.pure(clz, f)

  override def liftA2[A, B, C](f: (A, B) => C): (K1[K1[Writer.T, W], A], K1[K1[Writer.T, W], B]) => Writer[W, C] =
    (a1, a2) => {
      val i1 = Writer.narrow(a1)
      val i2 = Writer.narrow(a2)
      i2 ap (i1 fmap f.curried)
    }

  override def liftA3[A, B, C, D](f: (A, B, C) => D): (K1[K1[Writer.T, W], A], K1[K1[Writer.T, W], B], K1[K1[Writer.T, W], C]) => Writer[W, D] =
    (a1, a2, a3) => {
      val i1 = Writer.narrow(a1)
      val i2 = Writer.narrow(a2)
      val i3 = Writer.narrow(a3)
      i3 ap (i2 ap (i1 fmap f.curried))
    }
}
