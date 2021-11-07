package me.sakigamiyang.category4s.data.reader

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.Functor

trait ReaderFunctor[R] extends Functor[K1[Reader.T, R]] {
  override def fmap[A, B](f: A => B, v: K1[K1[Reader.T, R], A]): Reader[R, B] = {
    val reader = Reader.narrow(v)
    new Reader(r => f(reader.run(r)))
  }
}
