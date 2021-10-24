package me.sakigamiyang.category4s.types.monad

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.applicative.Applicative

trait Monad[T] extends Applicative[T] {
  def join[A](v: K1[T, K1[T, A]]): K1[T, A]

  def bind[A, B](f: A => K1[T, B], v: K1[T, A]): K1[T, B] = join(fmap(f, v))
}
