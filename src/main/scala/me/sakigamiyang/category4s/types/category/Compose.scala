package me.sakigamiyang.category4s.types.category

import me.sakigamiyang.category4s.K3

trait Compose[T, F] {
  def compose[A, B, C](f: K3[T, F, B, C], g: K3[T, F, A, B]): K3[T, F, A, C]
}
