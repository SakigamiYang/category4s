package me.sakigamiyang.category4s.data.reader

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.applicative.Applicative

trait ReaderApplicative[R] extends Applicative[K1[Reader.T, R]] {
  override def pure[A](v: A): Reader[R, A] = new Reader[R, A](_ => v)

  override def ap[A, B](f: K1[K1[Reader.T, R], A => B], v: K1[K1[Reader.T, R], A]): Reader[R, B] = {
    val reader = Reader.narrow(v)
    val readerf = Reader.narrow(f)
    new Reader(r => Reader.narrow(fmap(readerf.run(r), reader)).run(r))
  }
}
