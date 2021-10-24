package me.sakigamiyang.category4s.data.monoid

import me.sakigamiyang.category4s.types.group.Monoid

case class ListMonoid(value: List[_]) extends Monoid[List[_], ListMonoid] {
  def this() = this(List.empty)

  override def op(a: ListMonoid, b: ListMonoid): ListMonoid = ListMonoid(a.value ++ b.value)

  override def mzero(): ListMonoid = ListMonoid(List.empty)
}
