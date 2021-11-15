package me.sakigamiyang.category4s.data.writer

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.applicative.Applicative
import me.sakigamiyang.category4s.types.group.Monoid

trait WriterApplicative[W <: Monoid[Any, W]] extends Applicative[K1[Writer.T, W]] {
  val clz: Class[W]

  override def pure[A](v: A): Writer[W, A] = new Writer(clz, (v, Monoid.mzero(clz)))

  override def ap[A, B](f: K1[K1[Writer.T, W], A => B], v: K1[K1[Writer.T, W], A]): Writer[W, B] = {
    val writer = Writer.narrow(v)
    val writerf = Writer.narrow(f)
    val (value, w) = writerf.run
    val (value2, w2) = Writer.narrow(fmap(value, writer)).run
    new Writer(clz, (value2, Monoid.op(clz, w, w2)))
  }
}
