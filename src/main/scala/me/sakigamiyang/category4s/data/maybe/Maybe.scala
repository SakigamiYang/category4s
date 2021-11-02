package me.sakigamiyang.category4s.data.maybe

import me.sakigamiyang.category4s.K1

sealed abstract class Maybe[A] extends K1[Maybe.T, A] {
  // Monad
  private val monad = new MaybeMonad {}

  def fmap[B](f: A => B): Maybe[B] = monad.fmap(f, this)

  def ap[B](f: K1[Maybe.T, A => B]): Maybe[B] = monad.ap(f, this)

  def bind[B](f: A => K1[Maybe.T, B]): Maybe[B] = monad.bind(f, this)

  def getOrElse(v: A): A = this match {
    case Maybe.Just(value) => value
    case Maybe.None() => v
  }

  override def equals(obj: Any): Boolean = this match {
    case Maybe.Just(value) => obj match {
      case Maybe.Just(value2) => value == value2
      case _ => false
    }
    case Maybe.None() => obj match {
      case Maybe.None() => true
      case _ => false
    }
  }
}

object Maybe {
  class T

  final case class None[A]() extends Maybe[A]

  final case class Just[A](value: A) extends Maybe[A]

  def narrow[A](v: K1[T, A]): Maybe[A] = v.asInstanceOf

  // Monad
  private val monad = new MaybeMonad {}

  def pure[A](f: A): Maybe[A] = monad.pure(f)

  def join[A](v: K1[T, K1[T, A]]): Maybe[A] = monad.join(v)

  // ApplicativeOps
  private val applicativeOps = new MaybeApplicativeOps {}

  def liftA[A, B](f: A => B): K1[T, A] => Maybe[B] = applicativeOps.liftA(f)

  def liftA2[A, B, C](f: (A, B) => C): (K1[T, A], K1[T, B]) => Maybe[C] = applicativeOps.liftA2(f)

  def liftA3[A, B, C, D](f: (A, B, C) => D): (K1[T, A], K1[T, B], K1[T, C]) => Maybe[D] = applicativeOps.liftA3(f)

  // MonadOps
  private val monadOps = new MaybeMonadOps {}

  def liftM[A, B](f: A => B): K1[T, A] => K1[T, B] = monadOps.liftM(f)

  def liftM2[A, B, C](f: (A, B) => C): (K1[T, A], K1[T, B]) => K1[T, C] = monadOps.liftM2(f)

  def liftM3[A, B, C, D](f: (A, B, C) => D): (K1[T, A], K1[T, B], K1[T, C]) => K1[T, D] = monadOps.liftM3(f)
}
