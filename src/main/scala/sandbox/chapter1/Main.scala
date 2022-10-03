package sandbox.chapter1

import cats.implicits.{catsSyntaxEq, toShow}
import cats.{Eq, Show}
import sandbox.chapter1.introduction.PrintableInstances.stringPrint.format
import sandbox.chapter1.introduction.PrintableInstances.{catPrint, intPrint, stringPrint}
import sandbox.chapter1.introduction.PrintableSyntax.PrintableOps
import sandbox.chapter1.introduction.{Cat, PrintableImpl}

object Main extends App {
//object Main {
  // Intro exercise 1
  PrintableImpl.print("test")
  PrintableImpl.print(3)

  val cat = Cat("Salsa", 3, "blue")
  PrintableImpl.print(cat) // printed by PrintableImpl
  cat.print // Printed via PrintableSyntax

  // Meet Cats
  val showInt: Show[Int] = Show.apply[Int]
  println(showInt.show(123))
  val showString = "abc".show
  println(showString)

  println(format("hello"))

  // Exercise 2: Cat show
  implicit val catShow: Show[Cat] = Show.show(value => s"${value.name} is a ${value.age} year-old ${value.color} cat.")
  println(cat.show)

  // Exercise 3: Equality, Liberty and Felinity
  // implicit value below automatically creates === operator for both Option wrapped Cat class and plain Cat class
  implicit val catEq: Eq[Cat] = Eq.instance[Cat] {
    (cat1, cat2) =>
      cat1.age === cat2.age &&
        cat1.name === cat1.name &&
        cat1.color === cat1.color
  }
  
  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  println(cat1 === cat2)
  println(optionCat1 === optionCat2)
}