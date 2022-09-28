package sandbox.chapter2

import cats._
import cats.syntax.semigroup._
import cats.instances.option._

// Monoids and semigroups
object Main extends App {

  // Monoids must be associative and closed and have identity element
  // associativity: order doesn't matter ((a + b) +c) == (a + (b +c))
  // closed: Int + Int == Int
  // identity element: 0 + 0 = 0 so 0 is identity

  // Semigroup is Monoid without empty part

  // Excercise: The truth about monoids
  implicit val booleanAndMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) = a && b

      def empty = true
    }
  implicit def and(a: Boolean, b: Boolean): Monoid[Boolean] =
    new Monoid[Boolean] {
      override def empty: Boolean = true

      override def combine(x: Boolean, y: Boolean): Boolean = a && b
    }

  implicit val booleanOrMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) = a || b

      def empty = false
    }

  implicit val booleanXorMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) = (a && !b) || (!a && b)

      def empty = false
    }

  implicit val booleanXnorMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) = (!a || b) && (a || !b)

      def empty = false
    }
  println(booleanXnorMonoid.combine(false, false))

  // Exercise 2: All Set for Monoids
  implicit def setUnionMonoid[A](): Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]): Set[A] = a union b

      def empty = Set.empty[A]
    }

  // Doesn't compile for some reason.. moving forward
//  val intSetMonoid: Monoid[Set[Int]] = Monoid[Set[Int]]
//  val strSetMonoid = Monoid[Set[String]]
//  intSetMonoid.combine(Set(1, 2), Set(2, 3))

  // Exercise 3: Adding all the things
  def add[A](items: List[A])(implicit monoid: Monoid[A]): A = items.foldLeft(Monoid[A].empty)(_ |+| _)
  println(add(List(1,2,3)))
  println(add(List(Some(1),Some(1),None)))

  implicit val monoid: Monoid[Order] = new Monoid[Order] {
    def combine(o1: Order, o2: Order): Order =
      Order(
        o1.totalCost + o2.totalCost,
        o1.quantity + o2.quantity
      )
    def empty: Order = Order(0, 0)
  }

  println(add(List(Order(3,4), Order(1,2))))
}

case class Order(totalCost: Double, quantity: Double)
