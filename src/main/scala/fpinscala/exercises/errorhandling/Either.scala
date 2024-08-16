package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E,+A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] =
    this match
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    // [EE >: E, B], means that EE is a supertype of E  (supertype: Either and subtype: Left and Right)
    // f: A => Either[EE, B], means that f is a function that takes an A and returns an Either[EE, B]
    // return type is Either[EE, B], which means that the result of the flatMap operation is an Either with a type parameter of EE and B
    this match
      case Left(e) => Left(e)
      case Right(a) => f(a)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match
      case Left(_) => b
      case Right(a) => Right(a)


  /*map2 combines two Either values.
  If both are Right, it applies the given function to their contents and returns a Right with the result.
  If either one is a Left, it returns that Left (essentially propagating the error).*/
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for //The method body uses a for-comprehension which is a concise way to chain operations on monadic types like Either.
      a <- this //a <- this: Extracts the value a if this is a Right, or returns Left if this is a Left.
      b1 <- b //b1 <- b: Extracts the value b1 if b is a Right, or returns Left if b is a Left.
    yield f(a,b1)

object Either:
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = ???

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if xs.isEmpty then
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] = 
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B], f: (A, B) => C): Either[List[E], C] = ???

  def traverseAll[E, A, B](as: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] = ???

  def sequenceAll[E, A](as: List[Either[List[E], A]]): Either[List[E], List[A]] = ???
