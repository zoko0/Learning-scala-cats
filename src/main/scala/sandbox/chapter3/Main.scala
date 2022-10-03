package sandbox.chapter3

import cats.Functor
import cats.implicits.toFunctorOps
import sandbox.chapter1.introduction.Printable
import sandbox.chapter1.introduction.PrintableImpl.format
import sandbox.chapter1.introduction.PrintableInstances.intPrint

object Main extends App {

  // Exercise: Branching out with Functors
  val tree = Tree.leaf(100)
    .map(_ * 2)
    .map(value => s"$value!")
    .as("test")
  println(tree)
  Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2)

  val box = Box(42)
  println(format(box))
}

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  implicit val treeFunctor: Functor[Tree] =
    new Functor[Tree] {
      override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
        tree match {
          case Leaf(value) => Leaf(f(value))
          case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        }
    }
}

final case class Box[A](value: A)

object Box {
  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap[Box[A]](_.value)
}