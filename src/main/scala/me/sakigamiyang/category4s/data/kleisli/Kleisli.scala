package me.sakigamiyang.category4s.data.kleisli

import me.sakigamiyang.category4s.types.monad.Monad
import me.sakigamiyang.category4s.{K1, K2, K3}

class Kleisli[F, A, B](val m: Monad[F], val run: A => K1[F, B]) extends K3[Kleisli.T, F, A, B] {
  private def arrow(): KleisliArrow[F] = new KleisliArrow[F] {
    override val monad: Monad[F] = m
  }

  def compose[C](g: K3[Kleisli.T, F, B, C]): Kleisli[F, A, C] = arrow().compose(g, this)

  def first[C](): Kleisli[F, (A, C), (B, C)] = arrow().first(this)

  def second[C](): K3[Kleisli.T, F, (C, A), (C, B)] = arrow().second(this)

  def split[C, D](g: K3[Kleisli.T, F, C, D]): K3[Kleisli.T, F, (A, C), (B, D)] = arrow().split(this, g)

  def combine[C](g: K3[Kleisli.T, F, A, C]): K3[Kleisli.T, F, A, (B, C)] = arrow().combine(this, g)
}

object Kleisli {
  class T

  private def arrow[F](m: Monad[F]): KleisliArrow[F] = new KleisliArrow[F] {
    override val monad: Monad[F] = m
  }

  def narrow[F, A, B](v: K1[K2[T, F, A], B]): Kleisli[F, A, B] = v.asInstanceOf

  def id[F, A](monad: Monad[F]): Kleisli[F, A, A] = arrow(monad).id()

  def arr[F, A, B](monad: Monad[F], f: A => B): Kleisli[F, A, B] = arrow(monad).arr(f)

  def swap[F, A, B](monad: Monad[F]): K3[T, F, (A, B), (B, A)] = arrow(monad).swap
}
