package me.sakigamiyang.category4s.data.identity

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.monad.MonadOps

trait IdentityMonadOps extends MonadOps[Identity.T] with IdentityMonad {
  override def liftM[A, B](f: A => B): K1[Identity.T, A] => K1[Identity.T, B] =
    m => Identity.narrow(m) bind (x => Identity.pure(f(x)))

  override def liftM2[A, B, C](f: (A, B) => C): (K1[Identity.T, A], K1[Identity.T, B]) => K1[Identity.T, C] =
    (m1, m2) =>
      Identity.narrow(m1) bind (x1 =>
        Identity.narrow(m2) bind (x2 =>
          Identity.pure(f(x1, x2))))

  override def liftM3[A, B, C, D](f: (A, B, C) => D): (K1[Identity.T, A], K1[Identity.T, B], K1[Identity.T, C]) => K1[Identity.T, D] =
    (m1, m2, m3) =>
      Identity.narrow(m1) bind (x1 =>
        Identity.narrow(m2) bind (x2 =>
          Identity.narrow(m3) bind (x3 =>
            Identity.pure(f(x1, x2, x3)))))
}
