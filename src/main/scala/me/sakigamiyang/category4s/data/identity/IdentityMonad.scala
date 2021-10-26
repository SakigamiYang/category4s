package me.sakigamiyang.category4s.data.identity

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.monad.Monad

trait IdentityMonad extends Monad[Identity.T] with IdentityApplicative with IdentityFunctor {
  override def join[A](v: K1[Identity.T, K1[Identity.T, A]]): Identity[A] =
    Identity.narrow(Identity.narrow(v).value)

  override def bind[A, B](f: A => K1[Identity.T, B], v: K1[Identity.T, A]): Identity[B] =
    Identity.narrow(super.bind(f, v))
}
