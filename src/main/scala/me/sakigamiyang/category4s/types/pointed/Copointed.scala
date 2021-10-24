package me.sakigamiyang.category4s.types.pointed

import me.sakigamiyang.category4s.K1

trait Copointed[T] {
  def extract[A](v: K1[T, A]): A
}
