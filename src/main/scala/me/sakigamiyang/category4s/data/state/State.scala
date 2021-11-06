package me.sakigamiyang.category4s.data.state

import me.sakigamiyang.category4s.{K1, K2}

class State[S, A](val run: S => (A, S)) extends K2[State.T, S, A] {
  private val monad = new StateMonad[S] {}

  // Monad
  def fmap[B](f: A => B): State[S, B] = monad.fmap(f, this)

  def ap[B](f: State[S, A => B]): State[S, B] = monad.ap(f, this)

  def bind[B](f: A => State[S, B]): State[S, B] = monad.bind(f, this)

  def eval(s: S): A = run(s)._1

  def exec(s: S): S = run(s)._2
}

object State {
  class T

  def narrow[S, A](v: K1[K1[T, S], A]): State[S, A] = v.asInstanceOf

  // Monad
  private def monad[S]() = new StateMonad[S] {}

  def pure[S, A](v: A): State[S, A] = monad().pure(v)

  def join[S, A](v: K1[K1[T, S], K1[K1[T, S], A]]): State[S, A] = monad().join(v)


  // ApplicativeOps
  private def applicativeOps[S]() = new StateApplicativeOps[S] {}

  def liftA[S, A, B](f: A => B): K1[K1[State.T, S], A] => State[S, B] = applicativeOps[S]().liftA(f)

  def liftA2[S, A, B, C](f: (A, B) => C): (K1[K1[State.T, S], A], K1[K1[State.T, S], B]) => State[S, C] = applicativeOps[S]().liftA2(f)

  def liftA3[S, A, B, C, D](f: (A, B, C) => D): (K1[K1[State.T, S], A], K1[K1[State.T, S], B], K1[K1[State.T, S], C]) => State[S, D] = applicativeOps[S]().liftA3(f)

  // MonadOps
  private def monadOps[S]() = new StateMonadOps[S] {}

  def liftM[S, A, B](f: A => B): K1[K1[T, S], A] => K1[K1[State.T, S], B] = monadOps[S]().liftM(f)

  def liftM2[S, A, B, C](f: (A, B) => C): (K1[K1[T, S], A], K1[K1[T, S], B]) => K1[K1[State.T, S], C] = monadOps[S]().liftM2(f)

  def liftM3[S, A, B, C, D](f: (A, B, C) => D): (K1[K1[T, S], A], K1[K1[T, S], B], K1[K1[T, S], C]) => K1[K1[State.T, S], D] = monadOps[S]().liftM3(f)


  def get[S](): State[S, S] = new State[S, S]((s: S) => (s, s))

  def put[S](s: S): State[S, Unit] = new State[S, Unit](_ => (Unit, s))
}
