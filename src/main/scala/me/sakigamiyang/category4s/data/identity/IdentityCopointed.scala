package me.sakigamiyang.category4s.data.identity

import me.sakigamiyang.category4s.K1
import me.sakigamiyang.category4s.types.pointed.Copointed

trait IdentityCopointed extends Copointed[Identity.T] {
  override def extract[A](v: K1[Identity.T, A]): A = Identity.narrow(v).value
}
