package me.sakigamiyang.category4s.types.arrow

import me.sakigamiyang.category4s.K3
import me.sakigamiyang.category4s.types.category.Category

trait Arrow[T, F] extends Category[T, F] {
  def arr[A, B](f: A => B): K3[T, F, A, B]

  def swap[A, B]: K3[T, F, (A, B), (B, A)] = arr((pair: (A, B)) => (pair._2, pair._1))

  def first[A, B, C](f: K3[T, F, A, B]): K3[T, F, (A, C), (B, C)]

  def second[A, B, C](f: K3[T, F, A, B]): K3[T, F, (C, A), (C, B)] =
    compose(swap, compose(first(f), swap))

  def split[A, B, C, D](f: K3[T, F, A, B], g: K3[T, F, C, D]): K3[T, F, (A, C), (B, D)] =
    compose(second(g), first(f))

  def combine[A, B, C](f: K3[T, F, A, B], g: K3[T, F, A, C]): K3[T, F, A, (B, C)] =
    compose(split(f, g), arr(a => (a, a)))
}
