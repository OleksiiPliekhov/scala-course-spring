package karazin.scala.users.group.week3.homework

import karazin.scala.users.group.week3.{Functor, Monad}

object adt:

  case class ErrorOrT[F[_], A](value: F[ErrorOr[A]]):
    def flatMap[B](f: A => ErrorOrT[F, B])(using M: Monad[F]): ErrorOrT[F, B] =
      ErrorOrT(
        M.flatMap[ErrorOr[A], ErrorOr[B]](value) {
          case ErrorOr.Value(v) => f(v).value
          case ErrorOr.Error(e) => M.pure(ErrorOr.Error(e))
        }
      )

    def map[B](f: A => B)(using Functor: Functor[F]): ErrorOrT[F, B] =
      ErrorOrT(Functor.map(value)(_.map(f)))

    def withFilter(p: A => Boolean)(using Functor: Functor[F]): ErrorOrT[F, A] =
      ErrorOrT(Functor.map(value)(_.withFilter(p)))

  enum ErrorOr[+V]:

    case Value(v: V)

    case Error(e: Throwable)

    def flatMap[Q](f: V => ErrorOr[Q]): ErrorOr[Q] =
      this match
        case ErrorOr.Value(v) =>
          try
            f(v)
          catch
            case e: Throwable => ErrorOr.Error(e)

        case ErrorOr.Error(e) => ErrorOr.Error(e)

    def map[Q](f: V => Q): ErrorOr[Q] =
      this match
        case ErrorOr.Value(v) =>
          try
            ErrorOr.Value(f(v))
          catch
            case e: Throwable => ErrorOr.Error(e)

        case ErrorOr.Error(e) => ErrorOr.Error(e)

    def withFilter(p: V => Boolean): ErrorOr[V] =
      this match
        case ErrorOr.Value(v) =>
          try if p(v) then ErrorOr.Value(v) else ErrorOr.Error(new NoSuchElementException(s"Predicate does not hold for $v"))
          catch case e: Throwable => ErrorOr.Error(e)
        case ErrorOr.Error(e) => ErrorOr.Error(e)

    def flatten[U](using ev: V <:< ErrorOr[U]): ErrorOr[U] =
      this match
        case ErrorOr.Value(v) =>
          try ev(v)
          catch case e: Throwable => ErrorOr.Error(e)
        case ErrorOr.Error(e) => ErrorOr.Error(e)

    def foreach[U](f: V => U): Unit =
      this match
        case ErrorOr.Value(v) =>
          try f(v)
          catch case e: Throwable => ()
        case ErrorOr.Error(e) => ()

    def fold[Q](ifEmpty: => Q)(f: V => Q): Q =
      this match
        case ErrorOr.Value(v) =>
          try f(v)
          catch case e: Throwable => ifEmpty
        case ErrorOr.Error(e) => ifEmpty

    def foldLeft[Q](z: Q)(op: (Q, V) => Q): Q =
      this match
        case ErrorOr.Value(v) =>
          try op(z, v)
          catch case e: Throwable => z
        case ErrorOr.Error(e) => z

    def foldRight[Q](z: Q)(op: (V, Q) => Q): Q =
      this match
        case ErrorOr.Value(v) =>
          try op(v, z)
          catch case e: Throwable => z
        case ErrorOr.Error(e) => z


  object ErrorOr:

    def apply[V](v: V): ErrorOr[V] =
      try
        ErrorOr.Value(v)
      catch
        case e: Throwable => ErrorOr.Error(e)
