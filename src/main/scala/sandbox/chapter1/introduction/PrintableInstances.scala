package sandbox.chapter1.introduction

object PrintableInstances {
  implicit val stringPrint: Printable[String] = {
    (value: String) => value
  }
  implicit val intPrint: Printable[Int] = {
    (value: Int) => value.toString
  }
  implicit val catPrint: Printable[Cat] = {
    (value: Cat) => s"${value.name} is a ${value.age} year-old ${value.color} cat."
  }
}
