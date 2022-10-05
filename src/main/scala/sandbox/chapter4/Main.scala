package sandbox.chapter4

import cats.{Id, Monad}
import cats.instances.list._ // for Monad

object Main extends App {

  // Monads
  // informally monad is anything with a constructor and flapMap method
  // formally TBD
  // another definition from book (page 76): A monad is mechanism for sequencing computations.

  for {
    x <- (1 to 3).toList
    y <- (4 to 5).toList
  } yield (x, y)

  val list2 = Monad[List].
    flatMap(List(1, 2, 3)) { a => List(a, a * 10) }
  println(list2)
}

trait AMonad[F[_]] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  // Exercise solution below
  def map[A, B](value: F[A])(func: A => B): F[B] = flatMap(value)(a => pure(func(a)))
}

// Exercise: Monad for Id
trait IdMonad[A] {
  def pure(value: A): Id[A] = value

  def flatMap[B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)

  def map[B](value: Id[A])(func: A => B): Id[B] = func(value)
}