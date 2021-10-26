package me.sakigamiyang.category4s.data.identity

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.applicative.Applicative

trait IdentityApplicative extends Applicative[Identity.T] {
  override def pure[A](v: A): Identity[A] = Identity(v)

  override def ap[A, B](f: K1[Identity.T, A => B], v: K1[Identity.T, A]): Identity[B] =
    Identity.narrow(fmap(Identity.narrow(f).value, v))
}
