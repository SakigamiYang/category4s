package me.sakigamiyang.category4s.data.identity

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.applicative.ApplicativeOps

trait IdentityApplicativeOps extends ApplicativeOps[Identity.T] with IdentityMonad {
  override def liftA[A, B](f: A => B): K1[Identity.T, A] => Identity[B] =
    a => Identity.narrow(a) ap Identity.pure(f)


  override def liftA2[A, B, C](f: (A, B) => C): (K1[Identity.T, A], K1[Identity.T, B]) => Identity[C] =
    (a1, a2) => Identity.narrow(a2) ap (Identity.narrow(a1) fmap f.curried)


  override def liftA3[A, B, C, D](f: (A, B, C) => D): (K1[Identity.T, A], K1[Identity.T, B], K1[Identity.T, C]) => Identity[D] =
    (a1, a2, a3) => Identity.narrow(a3) ap (Identity.narrow(a2) ap (Identity.narrow(a1) fmap f.curried))
}
