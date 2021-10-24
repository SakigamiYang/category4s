package me.sakigamiyang.category4s.types.monad

import me.sakigamiyang.category4s.K1

trait MonadPlus[T] extends Monad[T] {
  def mzero[A]: K1[T, A]

  def mplus[A](m: K1[T, A]): K1[T, A]
}
