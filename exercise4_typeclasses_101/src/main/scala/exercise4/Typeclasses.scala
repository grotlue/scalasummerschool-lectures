package exercise4

object Typeclasses {

  // a) Define a type class Reversable which reverses values.

  trait Reversable[T] {
    def reverse(a: T): T
  }

  object Reversable {
    def apply[T](implicit impl: Reversable[T]): Reversable[T] = impl

    def reverse[T: Reversable](a: T): T = {
      Reversable[T].reverse(a)
    }
  }

  // b) Provide an instance of Reverse for String.

  implicit val reversableString = new Reversable[String] {
    def reverse(a: String): String = {
      a.reverse
    }
  }

  def testReversableString(str: String) = Reversable.reverse(str)

  // c) Write a type class named Smash that allows to smash values of the same type together.

  trait Smash[T] {
    def smash(a: T, b: T): T
  }

  object Smash {
    def apply[T](implicit impl: Smash[T]): Smash[T] = impl

    def smash[T: Smash](a: T, b: T): T = {
      Smash[T].smash(a, b)
    }
  }

  // d) Provide an instance for Smash for the Int and Double types.
  //    Use addition for the Int type and multiplication for the Double type.

  implicit val smashInt = new Smash[Int] {
    def smash(a: Int, b: Int): Int = a + b
  }

  implicit val smashDouble = new Smash[Double] {
    def smash(a: Double, b: Double): Double = a * b
  }

  def testSmashInt(a: Int, b: Int): Int = Smash.smash(a, b)

  def testSmashDouble(a: Double, b: Double): Double = Smash.smash(a, b)

  // e) Provide an instance for Smash for String type such that it concatenates the input.

  implicit val smashString = new Smash[String] {
    def smash(a: String, b: String): String = a + b
  }

  def testSmashString(a: String, b: String): String = Smash.smash(a, b)
}
