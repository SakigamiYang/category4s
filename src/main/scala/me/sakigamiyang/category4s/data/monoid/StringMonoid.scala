package me.sakigamiyang.category4s.data.monoid

import me.sakigamiyang.category4s.types.group.Monoid

case class StringMonoid(value: String) extends Monoid[String, StringMonoid] {
  def this() = this("")

  override def op(a: StringMonoid, b: StringMonoid): StringMonoid = StringMonoid(a.value + b.value)

  override def mzero(): StringMonoid = StringMonoid("")
}
