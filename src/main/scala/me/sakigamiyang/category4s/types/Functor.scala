package me.sakigamiyang.category4s.types

import me.sakigamiyang.category4s.K1

trait Functor[T] {
  def fmap[A, B](f: A => B, v: K1[T, A]): K1[T, B]
}
