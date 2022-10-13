package sandbox.chapter4

import cats.data.Writer
import cats.data.Reader
import cats.implicits.{catsSyntaxApplicativeErrorId, catsSyntaxApplicativeId, catsSyntaxEitherId, catsSyntaxWriterId}
import cats.instances.list._
import cats.{Eval, Id, Monad, MonadError}
import sandbox.chapter3.Tree.{branch, leaf}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future} // for Monad
import cats.syntax.functor._ // for map
import cats.syntax.flatMap._ // for flatMap

object Main {
//object Main extends App {

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
      ans <- if (n == 0) {
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

  // Exercise: Hacking on Readers
  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
    for {
      username <- findUsername(userId)
      passwordOk <- username.map { username => checkPassword(username, password)
      }.getOrElse {
        false.pure[DbReader]
      }
    }
    yield passwordOk
  }

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )
  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )
  val db = Db(users, passwords)
  checkLogin(1, "zerocool").run(db)
  checkLogin(4, "davinci").run(db)

  // Exercise: Post-Order Calculator

  import cats.data.State

  type CalcState[A] = State[List[Int], A]

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)
      case _ =>
        sys.error("Fail!")
    }

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans

  program.runA(Nil).value

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (a, b) => a.flatMap(_ => a.flatMap(_ => evalOne(b))) }

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

  evalInput("1 2 + 3 4 + *")

  // Exercise: Branching out Further with Monads
  branch(leaf(100), leaf(200)).
    flatMap(x => branch(leaf(x - 1), leaf(x + 1)))
  for {
    a <- branch(leaf(100), leaf(200))
    b <- branch(leaf(a - 10), leaf(a + 10))
    c <- branch(leaf(b - 1), leaf(b + 1))
  } yield c

  /*
  Summary for monads:
  Cats library defined monads: Option, Either, List, Future.
  Cats library custom data types: Id, Reader, Writer and State


   */
}

final case class Db(usernames: Map[Int, String], passwords: Map[String, String])


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