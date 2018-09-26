package exercise5

import cats.kernel._
import Comparison._

/** Short type class summary:
  *
  * trait Eq[A] {
  *   def eqv(x: A, y: A): Boolean = ???
  * }
  *
  * trait Order[A] {
  *   def compare(x: A, y: A): Int = ???
  * }
  *
  * trait Semigroup[A] {
  *   def combine(x: A, y: A): A = ???
  * }
  */
object Kernel {

  // a) Implement an implicit Eq[String] instance.

  implicit val eqvString = new Eq[String] {
    def eqv(x: String, y: String): Boolean = x == y
  }

  def testEqString(x: String, y: String): Boolean = Eq.eqv(x, y)

  // b) Implement an implicit Order[Int] instance.

  implicit val comparisionInt = new Order[Int] {
    def compare(x: Int, y: Int): Int = x - y
  }

  def testCompareInt(x: Int, y: Int): Comparison = Order.comparison(x, y)

  // c) Implement an implicit Semigroup[List[Int]] instance.

//  implicit val semiList = new Semigroup[List[Int]] {
//    def combine(x: List[Int], y: List[Int]): List[Int] = x ::: y
//  }

  def testCombineLists(x: List[Int], y: List[Int]): List[Int] =
    Semigroup.combine(x, y)

  // d) Update your List Semigroup such that it works for any element type, e.g. List[Int], List[String], ...

  implicit def semiListArb[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
    def combine(x: List[A], y: List[A]): List[A] = x ::: y
  }

  def testCombineListsArb[A](x: List[A], y: List[A]): List[A] =
    Semigroup.combine(x, y)
  // e) Can you find a type class in kernel which adds an empty element to Semigroup? How is it called?
}
