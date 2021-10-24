package me.sakigamiyang.category4s.types

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.group.Monoid

trait Foldable[T] {
  def foldLeft[A, B](initial: A, f: (A => B) => A, v: K1[T, A]): A

  def foldRight[A, B](initial: B, f: (A => A) => B, v: K1[T, B]): B

  def foldMap[A, M <: Monoid[T, M]](f: A => M, v: K1[T, A]): M
}
