package me.sakigamiyang.category4s.data.monoid

import me.sakigamiyang.category4s.types.group.Monoid

case class AnyMonoid(value: Boolean) extends Monoid[Boolean, AnyMonoid] {
  def this() = this(false)

  override def op(a: AnyMonoid, b: AnyMonoid): AnyMonoid = AnyMonoid(a.value || b.value)

  override def mzero(): AnyMonoid = AnyMonoid(false)
}
