package exercise5

import cats.Monad

import scala.annotation.tailrec

/** A Monad trait has the following abstract methods:
  *   new Monad[List] {
  *     def pure[A](a: A): List[A] = ???
  *
  *     def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = ???
  *
  *     // here you have to reduce `f(a)` to `List[B]` by ignoring `Left` values
  *     def tailRecM[A, B](a: A)(f: A => List[Either[A,B]]): List[B] = ???
  *   }
  */
object Monads {

  // a) Implement an implicit Monad[List] instance.
  implicit val listMonad = new Monad[List] {
    def pure[A](a: A): List[A] = List(a)

    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = {
      // Not sure if tailrec
//      f(a).foldLeft(List.empty[B]) { (acc, item) =>
//        item match {
//          case Right(v) => v +: acc
//          case _        => acc
//        }
//      }
      @tailrec
      def loop(list: List[Either[A, B]], acc: List[B]): List[B] = {
        list match {
          case Nil => acc
          case head :: tail =>
            head match {
              case Right(v) => loop(tail, v +: acc)
              case Left(_)  => loop(tail, acc)
            }
        }
      }

      loop(f(a), List.empty)
    }
  }

  def testListMonad[A, B](fa: List[A], f: A => List[B]): List[B] =
    Monad[List].flatMap(fa)(f)
}
