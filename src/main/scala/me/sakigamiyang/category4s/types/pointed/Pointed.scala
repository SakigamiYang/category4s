package me.sakigamiyang.category4s.types.pointed

import me.sakigamiyang.category4s.K1

trait Pointed[T] {
  def pure[A](v: A): K1[T, A]
}
