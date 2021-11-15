package me.sakigamiyang.category4s.data.writer

import me.sakigamiyang.category4s.types.group.Monoid
import me.sakigamiyang.category4s.{K1, K2}

class Writer[W <: Monoid[Any, W], A](val clazz: Class[W], val run: (A, W)) extends K2[Writer.T, W, A] {
  private val monad = new WriterMonad[W] {
    override val clz: Class[W] = clazz
  }

  // Monad
  def fmap[B](f: A => B): Writer[W, B] = monad.fmap(f, this)

  def ap[B](f: Writer[W, A => B]): Writer[W, B] = monad.ap(f, this)

  def bind[B](f: A => Writer[W, B]): Writer[W, B] = monad.bind(f, this)
}

object Writer {
  class T

  def narrow[W <: Monoid[Any, W], A](v: K1[K1[T, W], A]): Writer[W, A] = v.asInstanceOf

  // Monad
  private def monad[W <: Monoid[Any, W], A](clazz: Class[W]): WriterMonad[W] = new WriterMonad[W] {
    override val clz: Class[W] = clazz
  }

  def pure[W <: Monoid[Any, W], A](clz: Class[W], v: A): Writer[W, A] = monad(clz).pure(v)

  def join[W <: Monoid[Any, W], A](clz: Class[W], v: K1[K1[T, W], K1[K1[T, W], A]]): Writer[W, A] = monad(clz).join(v)

  // ApplicativeOps
  private def applicativeOps[W <: Monoid[Any, W]](clazz: Class[W]): WriterApplicativeOps[W] = new WriterApplicativeOps[W] {
    override val clz: Class[W] = clazz
  }

  def liftA[W <: Monoid[Any, W], A, B](clazz: Class[W], f: A => B): K1[K1[T, W], A] => Writer[W, B] = applicativeOps(clazz).liftA(f)

  def liftA2[W <: Monoid[Any, W], A, B, C](clazz: Class[W], f: (A, B) => C): (K1[K1[T, W], A], K1[K1[T, W], B]) => Writer[W, C] = applicativeOps(clazz).liftA2(f)

  def liftA3[W <: Monoid[Any, W], A, B, C, D](clazz: Class[W], f: (A, B, C) => D): (K1[K1[T, W], A], K1[K1[T, W], B], K1[K1[T, W], C]) => Writer[W, D] = applicativeOps(clazz).liftA3(f)

  // MonadOps
  private def monadOps[W <: Monoid[Any, W]](clazz: Class[W]): WriterMonadOps[W] = new WriterMonadOps[W] {
    override val clz: Class[W] = clazz
  }

  def liftM[W <: Monoid[Any, W], A, B](clazz: Class[W], f: A => B): K1[K1[T, W], A] => K1[K1[T, W], B] = monadOps(clazz).liftM(f)

  def liftM2[W <: Monoid[Any, W], A, B, C](clazz: Class[W], f: (A, B) => C): (K1[K1[T, W], A], K1[K1[T, W], B]) => K1[K1[T, W], C] = monadOps(clazz).liftM2(f)

  def liftM3[W <: Monoid[Any, W], A, B, C, D](clazz: Class[W], f: (A, B, C) => D): (K1[K1[T, W], A], K1[K1[T, W], B], K1[K1[T, W], C]) => K1[K1[T, W], D] = monadOps(clazz).liftM3(f)
}
