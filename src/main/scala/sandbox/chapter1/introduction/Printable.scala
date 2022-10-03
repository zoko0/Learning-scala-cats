package sandbox.chapter1.introduction

//1. Define a type class Printable[A] containing a single method format.
//  format should accept a value of type A and return a String.
//2. Create an object PrintableInstances containing instances of
//  Printable for String and Int.
//3. Define an object Printable with two generic interface methods:
//  format accepts a value of type A and a Printable of the correspond‐
//  ing type. It uses the relevant Printable to convert the A to a String.
//  print accepts the same parameters as format and returns Unit. It
//  prints the formatted A value to the console using println.

trait Printable[A] {
  def format(value: A): String

  // CH3 Exercise: Showing off with Contramap
  def contramap[B](func: B => A): Printable[B] =
    (value: B) => func(value).toString
}
