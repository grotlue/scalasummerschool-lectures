package exercise5

import cats.Applicative

/** An Applicative trait has the following abstract methods:
  *   new Applicative[List] {
  *     def pure[A](a: A): List[A] = ???
  *
  *     def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] = ???
  *   }
  */
object Applicatives {

  // a) Implement an implicit Applicative[List] instance.

  implicit val applicativeList = new Applicative[List] {
    def pure[A](x: A): List[A] = List(x)

    def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] = {
      for {
        a <- fa
        f <- ff
      } yield f(a)
    }
  }

  def testListApplicative[A, B](ff: List[A => B], fa: List[A]): List[B] =
    Applicative[List].ap(ff)(fa)

  // b) Apply all parameters to the function.

  def testApplyApp(f: Int => Int => String,
                   x: List[Int],
                   y: List[Int]): List[String] =
    Applicative[List].ap(x.map(f))(y)

//  Uglier but better solution as it uses the map method of Applicative and not of List,
//  so you are working with the same type. Usually you can use the <*> operator coming from cats
//  val appl = Applicative[List]
//  appl.ap(appl.map(x)(f))(y)
}
