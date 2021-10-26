package me.sakigamiyang.category4s.data.identity

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.monad.Comonad

trait IdentityComonad extends Comonad[Identity.T] with IdentityFunctor {
  override def extract[A](v: K1[Identity.T, A]): A = Identity.narrow(v).value

  override def duplicate[A](v: K1[Identity.T, A]): Identity[K1[Identity.T, A]] = Identity(Identity.narrow(v))

  override def extend[A, B](f: K1[Identity.T, A] => B, v: K1[Identity.T, A]): Identity[B] =
    Identity.narrow(fmap(f, duplicate(v)))
}
