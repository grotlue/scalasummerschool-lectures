package exercise3

import scala.util.Try

object AdtsSolution {

  def getNth(list: List[Int], n: Int): Option[Int] = list.drop(n).headOption

  def double(n: Option[Int]): Option[Int] = n.map(_ * 2)

  def isEven(n: Int): Either[String, Int] =
    if (n % 2 == 0) Right(n)
    else            Left("Not an even number.")

  def safeDivide(a: Int, b: Int): Either[String, Int] =
    if (b == 0) Left("You cannot divide by zero.")
    else        Right(a / b)

  def goodOldJava(impure: String => Int, str: String): Try[Int] =
    Try(impure(str)).recover {
      case _ => 0
    }
}
