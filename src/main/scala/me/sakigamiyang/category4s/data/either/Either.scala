package me.sakigamiyang.category4s.data.either

import me.sakigamiyang.category4s.{K1, K2}

sealed abstract class Either[L, R] extends K2[Either.T, L, R] {
  // Monad
  private val monad = new EitherMonad[L] {}

  def fmap[S](f: R => S): Either[L, S] = monad.fmap(f, this)

  def ap[S](f: Either[L, R => S]): Either[L, S] = monad.ap(f, this)

  def bind[S](f: R => Either[L, S]): Either[L, S] = monad.bind(f, this)

  override def equals(obj: Any): Boolean =
    obj match {
      case Either.Left(value) =>
        this match {
          case Either.Left(value2) => value == value2
          case _ => false
        }
      case Either.Right(value) =>
        this match {
          case Either.Right(value2) => value == value2
          case _ => false
        }
      case _ => false
    }
}

object Either {
  class T

  final case class Left[L, R](value: L) extends Either[L, R]

  final case class Right[L, R](value: R) extends Either[L, R]

  // Arrow
  def narrow[L, R](v: K1[K1[Either.T, L], R]): Either[L, R] = v.asInstanceOf

  // Monad
  private def monad[L](): EitherMonad[L] = new EitherMonad[L] {}

  def pure[L, S](v: S): Either[L, S] = monad().pure(v)

  def join[L, S](v: K1[K1[T, L], K1[K1[T, L], S]]): Either[L, S] = monad().join(v)

  // ApplicativeOps
  private def applicativeOps[S](): EitherApplicativeOps[S] = new EitherApplicativeOps[S] {}

  def liftA[S, A, B](f: A => B): K1[K1[T, S], A] => Either[S, B] = applicativeOps().liftA(f)

  def liftA2[S, A, B, C](f: (A, B) => C): (K1[K1[T, S], A], K1[K1[T, S], B]) => Either[S, C] = applicativeOps().liftA2(f)

  def liftA3[S, A, B, C, D](f: (A, B, C) => D): (K1[K1[T, S], A], K1[K1[T, S], B], K1[K1[T, S], C]) => Either[S, D] = applicativeOps().liftA3(f)

  // MonadOps
  private def monadOps[S](): EitherMonadOps[S] = new EitherMonadOps[S] {}

  def liftM[S, A, B](f: A => B): K1[K1[T, S], A] => K1[K1[T, S], B] = monadOps().liftM(f)

  def liftM2[S, A, B, C](f: (A, B) => C): (K1[K1[T, S], A], K1[K1[T, S], B]) => K1[K1[T, S], C] = monadOps().liftM2(f)

  def liftM3[S, A, B, C, D](f: (A, B, C) => D): (K1[K1[T, S], A], K1[K1[T, S], B], K1[K1[T, S], C]) => K1[K1[T, S], D] = monadOps().liftM3(f)

  override def equals(obj: Any): Boolean = super.equals(obj)
}
