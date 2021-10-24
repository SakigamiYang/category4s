package me.sakigamiyang.category4s.types.monad

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.Functor
import me.sakigamiyang.category4s.types.pointed.Copointed

trait Comonad[T] extends Copointed[T] with Functor[T] {
  def duplicate[A](v: K1[T, A]): K1[T, K1[T, A]]

  def extend[A, B](f: K1[T, A] => B, v: K1[T, A]): K1[T, B] = fmap(f, duplicate(v))
}
