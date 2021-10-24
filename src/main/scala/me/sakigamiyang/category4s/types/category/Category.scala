package me.sakigamiyang.category4s.types.category

import me.sakigamiyang.category4s.K3

trait Category[T, F] extends Compose[T, F] {
  def id[A](): K3[T, F, A, A]
}
