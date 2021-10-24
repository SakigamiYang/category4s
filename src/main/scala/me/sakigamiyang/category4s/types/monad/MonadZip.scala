package me.sakigamiyang.category4s.types.monad

import me.sakigamiyang.category4s.K1

trait MonadZip[T] extends Monad[T] {
  def mzip[A, B](m1: K1[T, A], m2: K1[T, B]): K1[T, (A, B)]

  def mzipWith[A, B, C](m1: K1[T, A], m2: K1[T, B], f: (A, B) => C): K1[T, C]

  def munzip[A, B](m: K1[T, (A, B)]): (K1[T, A], K1[T, B])

}
