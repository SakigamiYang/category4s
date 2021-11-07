package me.sakigamiyang.category4s.data.reader

import me.sakigamiyang.category4s.{K1, K2}

class Reader[R, A](val run: R => A) extends K2[Reader.T, R, A] {
  // Monad
  private val monad = new ReaderMonad[R] {}

  def fmap[B](f: A => B): Reader[R, B] = monad.fmap(f, this)

  def ap[B](f: Reader[R, A => B]): Reader[R, B] = monad.ap(f, this)

  def bind[B](f: A => Reader[R, B]): Reader[R, B] = monad.bind(f, this)

  def local(f: R => R): Reader[R, A] = new Reader[R, A](r => run(f(r)))
}

object Reader {
  class T

  def narrow[R, A](v: K1[K1[T, R], A]): Reader[R, A] = v.asInstanceOf

  // Monad
  private def monad[R]() = new ReaderMonad[R] {}

  def pure[R, A](v: A): Reader[R, A] = monad[R]().pure(v)

  def join[R, A](v: K1[K1[T, R], K1[K1[T, R], A]]): Reader[R, A] = monad[R]().join(v)

  // ApplicativeOps
  private def applicativeOps[R]() = new ReaderApplicativeOps[R] {}

  def liftA[R, A, B](f: A => B): K1[K1[T, R], A] => Reader[R, B] = applicativeOps[R]().liftA(f)

  def liftA2[R, A, B, C](f: (A, B) => C): (K1[K1[T, R], A], K1[K1[T, R], B]) => Reader[R, C] = applicativeOps[R]().liftA2(f)

  def liftA3[R, A, B, C, D](f: (A, B, C) => D): (K1[K1[T, R], A], K1[K1[T, R], B], K1[K1[T, R], C]) => Reader[R, D] = applicativeOps[R]().liftA3(f)

  // MonadOps
  private def monadOps[R]() = new ReaderMonadOps[R] {}

  def liftM[R, A, B](f: A => B): K1[K1[T, R], A] => K1[K1[T, R], B] = monadOps[R]().liftM(f)

  def liftM2[R, A, B, C](f: (A, B) => C): (K1[K1[T, R], A], K1[K1[T, R], B]) => K1[K1[T, R], C] = monadOps[R]().liftM2(f)

  def liftM3[R, A, B, C, D](f: (A, B, C) => D): (K1[K1[T, R], A], K1[K1[T, R], B], K1[K1[T, R], C]) => K1[K1[T, R], D] = monadOps[R]().liftM3(f)

  def ask[R](): Reader[R, R] = new Reader[R, R]((r: R) => r)
}
