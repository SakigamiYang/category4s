package me.sakigamiyang.category4s

/**
 * In functional programming, you can create a `Maybe<T>`.
 * This is a generic type which means that
 * you can define a `Maybe<Int>` or a `Maybe<String>`
 * which are conceptually different types.
 * <p>
 * You can see the `Maybe<T>` as a way to
 * create different types by changing the value of the type parameter T.
 * This is a type constructor, where the input parameter is the generic type,
 * and the result is a specific type.
 * <p>
 * Arrow represents the idea of type constructors using higher kinds,
 * which are a mechanism introduced in Scala.
 * They represent an abstraction of all type constructors.
 * The idea is to consider a type like `Maybe<T>`
 * as a specific `Kind<T, F>` where F is Maybe and T is the usual parameter type.
 */

trait K1[T, A] {}

trait K2[T, A, B] extends K1[K1[T, A], B] {}

trait K3[T, A, B, C] extends K1[K2[T, A, B], C] {}

trait K4[T, A, B, C, D] extends K1[K3[T, A, B, C], D] {}

trait K5[T, A, B, C, D, E] extends K1[K4[T, A, B, C, D], E] {}
