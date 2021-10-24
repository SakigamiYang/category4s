package me.sakigamiyang.category4s.types.applicative

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.Functor
import me.sakigamiyang.category4s.types.pointed.Pointed

trait Applicative[T] extends Functor[T] with Pointed[T] {
  def ap[A, B](f: K1[T, A => B], v: K1[T, A]): K1[T, B]

}
