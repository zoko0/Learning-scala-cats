package sandbox.chapter4

import cats.data.Writer
import cats.implicits.{catsSyntaxApplicativeErrorId, catsSyntaxApplicativeId, catsSyntaxEitherId, catsSyntaxWriterId}
import cats.instances.list._
import cats.{Eval, Id, Monad, MonadError}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future} // for Monad

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

  // Either
  val a = 3.asRight[String]
  println(a)
  val b = "123".asLeft[Int]
  for {
    x <- a
    y <- b
  } yield x + y

  // Exercise
  // Either can be used for:
  // Error recovery - retries strategies
  // Error reporting - logging
  // ..

  // Another exercise
  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
    if (age >= 18) age.pure[F]
    else new IllegalArgumentException("Age must be greater than or equal to 18").raiseError[F, Int]


  // Eval monad
  // Three different combinations of eval mode:
  // call-by-value - val -> eager and memoized -> Eval.now
  // call-by-name - def -> lazy and not memoized -> Eval.always
  // call-by-need - lazy val -> lazy and memoized -> Eval.later
  val ans = for {
    a <- Eval.later {
      println("Calculating A");
      40
    }
    b <- Eval.always {
      println("Calculating B");
      2
    }
  } yield {
    println("Adding A and B")
    a + b
  }
  println(ans.value)

  // Exercise
  def foldRightEval[A, B](as: List[A], acc: Eval[B])
                         (fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil =>
        acc
    }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }.value

  println(foldRight((1 to 100000).toList, 0L)(_ + _))

  // Exercise: Show your Working
  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)


  type Logged[A] = Writer[Vector[String], A]

  def factorial(n: Int): Logged[Int] =
    for {
      ans <- if(n == 0) {
        1.pure[Logged]
      } else {
        slowly(factorial(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

  Await.result(Future.sequence(Vector(
    Future(factorial(5)),
    Future(factorial(5))
  )).map(_.map(_.written)), 5.seconds)

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