package me.sakigamiyang.category4s.data.kleisli

import me.sakigamiyang.category4s.types.category.Category

trait KleisliCategory[F] extends Category[Kleisli.T, F] with KleisliCompose[F] {
  override def id[A](): Kleisli[F, A, A] = new Kleisli(monad, a => monad.pure(a))
}
