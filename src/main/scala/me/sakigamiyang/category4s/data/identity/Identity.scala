package me.sakigamiyang.category4s.data.identity

import me.sakigamiyang.category4s.K1

case class Identity[A](value: A) extends K1[Identity.T, A] {
  // Monad
  private val monad = new IdentityMonad {}

  def fmap[B](f: A => B): Identity[B] = monad.fmap(f, this)

  def ap[B](f: Identity[A => B]): Identity[B] = monad.ap(f, this)

  def bind[B](f: A => Identity[B]): Identity[B] = monad.bind(f, this)

  // MonadZip
  private val monadZip = new IdentityMonadZip {}

  def mzip[B](m: K1[Identity.T, B]): Identity[(A, B)] = monadZip.mzip(this, m)

  def mzipWith[B, C](m: K1[Identity.T, B], f: (A, B) => C): Identity[C] = monadZip.mzipWith(this, m, f)

  // Comonad

  private val comonad = new IdentityComonad {}

  def extract(): A = comonad.extract(this)

  def duplicate(): Identity[K1[Identity.T, A]] = comonad.duplicate(this)

  def extend[B](f: K1[Identity.T, A] => B): Identity[B] = comonad.extend(f, this)
}

object Identity {
  class T

  def narrow[A](i: K1[T, A]): Identity[A] = i.asInstanceOf

  // Monad
  private val monad = new IdentityMonad {}

  def pure[A](v: A): Identity[A] = monad.pure(v)

  def join[A](v: K1[T, K1[T, A]]): Identity[A] = monad.join(v)

  // ApplicativeOps
  private val applicativeOps = new IdentityApplicativeOps {}

  def liftA[A, B](f: A => B): K1[T, A] => Identity[B] = applicativeOps.liftA(f)

  def liftA2[A, B, C](f: (A, B) => C): (K1[T, A], K1[T, B]) => Identity[C] = applicativeOps.liftA2(f)

  def liftA3[A, B, C, D](f: (A, B, C) => D): (K1[T, A], K1[T, B], K1[T, C]) => Identity[D] = applicativeOps.liftA3(f)

  // MonadOps
  private val monadOps = new IdentityMonadOps {}

  def liftM[A, B](f: A => B): K1[T, A] => K1[T, B] = monadOps.liftM(f)

  def liftM2[A, B, C](f: (A, B) => C): (K1[T, A], K1[T, B]) => K1[T, C] = monadOps.liftM2(f)

  def liftM3[A, B, C, D](f: (A, B, C) => D): (K1[T, A], K1[T, B], K1[T, C]) => K1[T, D] = monadOps.liftM3(f)

  // MonadZip
  private val monadZip = new IdentityMonadZip {}

  def munzip[A, B](m: K1[T, (A, B)]): (Identity[A], Identity[B]) = monadZip.munzip(m)
}
