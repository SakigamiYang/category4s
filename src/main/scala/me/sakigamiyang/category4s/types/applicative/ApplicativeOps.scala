package me.sakigamiyang.category4s.types.applicative

import me.sakigamiyang.category4s.K1

trait ApplicativeOps[T] extends Applicative[T] {
  def liftA[A, B](f: A => B): K1[T, A] => K1[T, B]

  def liftA2[A, B, C](f: (A, B) => C): (K1[T, A], K1[T, B]) => K1[T, C]

  def liftA3[A, B, C, D](f: (A, B, C) => D): (K1[T, A], K1[T, B], K1[T, C]) => K1[T, D]

}
