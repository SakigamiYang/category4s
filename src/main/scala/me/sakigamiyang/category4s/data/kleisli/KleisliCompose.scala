package me.sakigamiyang.category4s.data.kleisli

import me.sakigamiyang.category4s.K3
import me.sakigamiyang.category4s.types.category.Compose
import me.sakigamiyang.category4s.types.monad.Monad

trait KleisliCompose[F] extends Compose[Kleisli.T, F] {
  val monad: Monad[F]

  override def compose[A, B, C](f: K3[Kleisli.T, F, B, C], g: K3[Kleisli.T, F, A, B]): Kleisli[F, A, C] = {
    new Kleisli(
      monad,
      (a: A) => monad.bind(Kleisli.narrow(f).run, monad.bind(Kleisli.narrow(g).run, monad.pure(a))))
  }
}
