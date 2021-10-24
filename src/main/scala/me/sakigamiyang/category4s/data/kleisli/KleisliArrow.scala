package me.sakigamiyang.category4s.data.kleisli

import me.sakigamiyang.category4s.K3
import me.sakigamiyang.category4s.types.arrow.Arrow

trait KleisliArrow[F] extends Arrow[Kleisli.T, F] with KleisliCategory[F] {
  override def arr[A, B](f: A => B): Kleisli[F, A, B] =
    new Kleisli(monad, a => monad.pure(a))

  override def first[A, B, C](f: K3[Kleisli.T, F, A, B]): Kleisli[F, (A, C), (B, C)] =
    new Kleisli(
      monad,
      pair => monad.fmap({ (b: B) => (b, pair._2) }, Kleisli.narrow(f).run(pair._1)))
}
