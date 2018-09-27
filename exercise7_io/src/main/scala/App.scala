import cats.effect.{IO, IOApp, ExitCode}

import scala.util.Try
import scala.io.{Source, StdIn}

import java.io.BufferedReader

/**
  * This app is an cli tool reading a user database (file) from disk which stores user entries as follows:
  *
  *     // name, age
  *     Gandalf,2019
  *     Frodo,31
  *     ...
  *
  * The path to this file is given during application start:
  *
  *   sbt> project io-exercises
  *   sbt> run "/Users/paul.heymann/Private/lectures/exercise7_io/user.db"
  *
  * After reading and parsing the file the app is waiting for the user to search for a name in the database.
  * All results are printed to the console in the end.
  *
  * You can exit the program by tipping 'q'.
  */
/**
  * These IO methods might come in handy:
  *   IO.pure[A](a: A): IO[A]
  *
  *   IO(...).attempt: Either[Throwable, A]
  *
  *   IO(...).bracket[B](use: A => IO[B])(release: A => IO[Unit]): IO[B]
  *
  *   IO(...).unsafeRunSunc or use >>> IOApp <<<
  */
object App {

  case class User(name: String, age: Int)

  case class ParseError(msg: String) extends Exception

  /** Opens a file and hands us a reader which acts like a reference to the file.
    *
    * @param path path to the user db file
    * @return reference to the db file
    */
  def readDbFile(path: String): IO[BufferedReader] = {
    IO(new BufferedReader(Source.fromFile(path).reader))
  }

  /** Transforms a raw line to a [[User]] class.
    *
    * @param line user encoded as [[String]]
    * @return [[Right]] providing a [[User]] if the [[String]] is valid, else [[Left]] with [[ParseError]]
    */
  def parse(line: String): Either[Throwable, User] = {
    line.split(',') match {
      case Array(name, ageStr) =>
        Try(ageStr.toInt).toEither.map(age => User(name, age))
      case _ => Left(ParseError(s"invalid user line format: $line"))
    }
  }

  /** This functions composes [[readDbFile]] and [[parse]] to transform all user entries to a
    * [[List[User]]].
    *
    * @param path path to the user db file
    * @return [[Right]] of [[List[User]] if the whole file is valid, else [[Left]] with [[ParseError]]
    */
  def loadDb(path: String): IO[List[User]] = {
    val buffer = readDbFile(path)

    buffer
      .bracket { in =>
        IO {
          def loop(agg: List[User]): Either[Throwable, List[User]] = {
            val line = in.readLine()

            // buffer.readLine() return `null` if new line is left in the file
            if (line != null) parse(line).flatMap(user => loop(user :: agg))
            else Right(agg.reverse)
          }

          loop(Nil)
        }
      } { in =>
        IO(in.close())
      }
      .flatMap(IO.fromEither)
  }

  def showResult(users: List[User]): String = {
    users
      .map(user => user.name + "," + user.age)
      .mkString("----\n", "\n", "\n----\n")
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      // stop application if do not provide a user db path
      System.exit(1)
    } else {
      val dbPath = args(0)

      while (true) {
        val name = StdIn.readLine("Search name or exit (q): ")

        val usersList = loadDb(dbPath)

        // stop application if the user inputs 'q'
        if (name == "q") System.exit(0)

        val result = usersList.flatMap(users =>
          IO(users.filter(_.name.toLowerCase == name.toLowerCase)))

        println(result.flatMap(r => IO(showResult(r))))
      }
    }
  }
}
