package exercise5

import cats.Functor

/** A Functor trait has the following abstract method:
  *   new Functor[List] {
  *     def map[A, B](fa: List[A])(f: A => B): List[B]
  *   }
  */
object Functors {

  // a) Implement implicit Functor[List] instance.
  implicit val functorList: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = { fa.map(item => f(item)) }
  }

  def testListFunctor[A, B](fa: List[A], f: A => B): List[B] =
    Functor[List].fmap(fa)(f)
}
