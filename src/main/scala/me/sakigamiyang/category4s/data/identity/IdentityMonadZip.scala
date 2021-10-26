package me.sakigamiyang.category4s.data.identity

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.monad.MonadZip

trait IdentityMonadZip extends MonadZip[Identity.T] with IdentityMonad {
  override def mzip[A, B](m1: K1[Identity.T, A], m2: K1[Identity.T, B]): Identity[(A, B)] =
    Identity.pure((Identity.narrow(m1).value, Identity.narrow(m2).value))

  override def mzipWith[A, B, C](m1: K1[Identity.T, A], m2: K1[Identity.T, B], f: (A, B) => C): Identity[C] =
    Identity.pure(f(Identity.narrow(m1).value, Identity.narrow(m2).value))

  override def munzip[A, B](m: K1[Identity.T, (A, B)]): (Identity[A], Identity[B]) = {
    val (first, second) = Identity.narrow(m).value
    (Identity.pure(first), Identity.pure(second))
  }
}
