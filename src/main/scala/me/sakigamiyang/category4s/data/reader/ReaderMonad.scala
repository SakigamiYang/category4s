package me.sakigamiyang.category4s.data.reader

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.monad.Monad

trait ReaderMonad[R] extends Monad[K1[Reader.T, R]] with ReaderApplicative[R] with ReaderFunctor[R] {
  override def join[A](v: K1[K1[Reader.T, R], K1[K1[Reader.T, R], A]]): Reader[R, A] = {
    val reader = Reader.narrow(v)
    new Reader(r => Reader.narrow(reader.run(r)).run(r))
  }

  override def bind[A, B](f: A => K1[K1[Reader.T, R], B], v: K1[K1[Reader.T, R], A]): Reader[R, B] =
    Reader.narrow(super.bind(f, v))
}
