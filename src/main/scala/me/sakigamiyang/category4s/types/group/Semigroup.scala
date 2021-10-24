package me.sakigamiyang.category4s.types.group

trait Semigroup[A, Self <: Semigroup[A, Self]] {
  def op(a: Self, b: Self): Self

}

object Semigroup {
  def op[A <: Semigroup[Any, A]](clazz: Class[A], a: A, b: A): A =
    clazz.getDeclaredConstructor().newInstance().op(a, b)
}
