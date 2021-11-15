package me.sakigamiyang.category4s.data.writer

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.Functor
import me.sakigamiyang.category4s.types.group.Monoid

trait WriterFunctor[W <: Monoid[Any, W]] extends Functor[K1[Writer.T, W]] {
  val clz: Class[W]

  override def fmap[A, B](f: A => B, v: K1[K1[Writer.T, W], A]): Writer[W, B] = {
    val writer = Writer.narrow(v)
    val (value, w) = writer.run
    new Writer(clz, (f(value), w))
  }
}
