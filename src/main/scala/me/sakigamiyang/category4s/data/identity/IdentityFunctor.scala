package me.sakigamiyang.category4s.data.identity

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.Functor

trait IdentityFunctor extends Functor[Identity.T] {
  override def fmap[A, B](f: A => B, v: K1[Identity.T, A]): Identity[B] =
    Identity(f(Identity.narrow(v).value))
}
