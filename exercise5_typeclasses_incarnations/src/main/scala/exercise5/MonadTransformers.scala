package exercise5

import cats.instances.all.catsStdInstancesForList
import cats.Monad
import scala.annotation.tailrec

/** Implement a Monad instance for OptionT[F[_], A]. Use the kind-projector `?` when writing `new Monad[OptionT[F, ?]]`.
  */
object MonadTransformers {

  type OptionT[F[_], A] = F[Option[A]]

  implicit def optionT[F[_]: Monad] =
    new Monad[OptionT[F, ?]] {
      def pure[A](a: A): OptionT[F, A] = Monad[F].pure(Some(a))

      def flatMap[A, B](fa: OptionT[F, A])(
          f: A => OptionT[F, B]): OptionT[F, B] =
        Monad[F].flatMap(fa) {
          case Some(v) => f(v)
          case None    => Monad[F].pure(None)
        }

      def tailRecM[A, B](a: A)(
          f: A => OptionT[F, Either[A, B]]): OptionT[F, B] = {

        flatMap(f(a))(item =>
          item match {
            case Right(v)   => pure(v)
            case Left(newA) => tailRecM(newA)(f)
        })
      }
    }

  def testOptionTransformer[A, B](fa: OptionT[List, A],
                                  f: A => OptionT[List, B]): OptionT[List, B] =
    Monad[OptionT[List, ?]].flatMap(fa)(f)
}
