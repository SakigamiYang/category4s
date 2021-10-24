package me.sakigamiyang.category4s.types.monad

import me.sakigamiyang.category4s.K1

trait MonadOps[T] extends Monad[T] {
  def liftM[A, B](f: A => B): K1[T, A] => K1[T, B]

  def liftM2[A, B, C](f: (A, B) => C): (K1[T, A], K1[T, B]) => K1[T, C]

  def liftM3[A, B, C, D](f: (A, B, C) => D): (K1[T, A], K1[T, B], K1[T, C]) => K1[T, D]
}
