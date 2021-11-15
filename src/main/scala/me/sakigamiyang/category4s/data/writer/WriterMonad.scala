package me.sakigamiyang.category4s.data.writer

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.group.Monoid
import me.sakigamiyang.category4s.types.monad.Monad

trait WriterMonad[W <: Monoid[Any, W]] extends Monad[K1[Writer.T, W]] with WriterApplicative[W] with WriterFunctor[W] {
  override def join[A](v: K1[K1[Writer.T, W], K1[K1[Writer.T, W], A]]): Writer[W, A] = {
    val writer = Writer.narrow(v)
    val (value, w) = writer.run
    val (value2, w2) = Writer.narrow(value).run
    new Writer(clz, (value2, Monoid.op(clz, w, w2)))
  }

  override def bind[A, B](f: A => K1[K1[Writer.T, W], B], v: K1[K1[Writer.T, W], A]): Writer[W, B] =
    Writer.narrow(super.bind(f, v))
}
