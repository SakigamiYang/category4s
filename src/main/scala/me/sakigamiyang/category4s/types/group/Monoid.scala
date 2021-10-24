package me.sakigamiyang.category4s.types.group

trait Monoid[A, Self <: Semigroup[A, Self]] extends Semigroup[A, Self] {
  def mzero(): Self
}

object Monoid {
  def op[A <: Monoid[Any, A]](clazz: Class[A], a: A, b: A): A =
    clazz.getDeclaredConstructor().newInstance().op(a, b)
}
