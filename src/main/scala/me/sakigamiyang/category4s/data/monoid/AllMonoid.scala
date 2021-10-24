package me.sakigamiyang.category4s.data.monoid

import me.sakigamiyang.category4s.types.group.Monoid

case class AllMonoid(value: Boolean) extends Monoid[Boolean, AllMonoid] {
  def this() = this(true)

  override def op(a: AllMonoid, b: AllMonoid): AllMonoid = AllMonoid(a.value && b.value)

  override def mzero(): AllMonoid = AllMonoid(true)
}
