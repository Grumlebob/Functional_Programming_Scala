package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
   * which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail *))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1, 2, 3, 4, 5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x, y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, (x, y) => x * y) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match
      case Nil => sys.error("tail of empty list")
      case Cons(_, t) => t

  def setHead[A](l: List[A], h: A): List[A] =
    l match
      case Nil => sys.error("setHead on empty list")
      case Cons(_, t) => Cons(h, t)

  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if n <= 0 then l
    else l match
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l

  def init[A](l: List[A]): List[A] =
    l match
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))

  def length[A](l: List[A]): Int =
    l match
      case Nil => 0
      case Cons(_, t) => 1 + length(t)


  def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B =
    l match
      case Nil => acc
      case Cons(h, t) =>
        val newAcc = f(acc, h)
        foldLeft(t, newAcc, f)

  def sumViaFoldLeft(ns: List[Int]): Int =
    def add(acc: Int, curr_element: Int): Int = acc + curr_element
    val acc = 0
    foldLeft(ns, acc, add)

  def productViaFoldLeft(ns: List[Double]): Double =
    def product(acc: Double, curr_element: Double): Double = acc * curr_element
    val acc = 1
    foldLeft(ns, acc, product)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    def lenght(acc: Int, curr_element: A): Int = 1 + acc
    val acc = 0
    foldLeft(l, acc, lenght)

  def reverse[A](l: List[A]): List[A] =
    def reverseList(acc: List[A], curr_element: A): List[A] = Cons(curr_element, acc)
    val acc = Nil
    foldLeft(l, acc, reverseList)

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    def appendList(acc: List[A], curr_element: A): List[A] = Cons(curr_element, acc)
    foldRight(l, r, Cons(_,_)) //Vi giver vores start liste, og som acc giver vi den liste vi vil appende til

  def concat[A](l: List[List[A]]): List[A] =
    def appendList(acc: List[A], curr_element: List[A]): List[A] = appendViaFoldRight(acc, curr_element)
    foldLeft(l, Nil, appendList)

  def incrementEach(l: List[Int]): List[Int] =
    def increment(acc: List[Int], curr_element: Int): List[Int] = Cons(curr_element + 1, acc)
    val acc = Nil
    val result = foldLeft(l, acc, increment)
    reverse(result)

  def doubleToString(l: List[Double]): List[String] =
    def doubleToString(acc: List[String], curr_element: Double): List[String] = Cons(curr_element.toString, acc)
    val acc = Nil
    val result = foldLeft(l, acc, doubleToString)
    reverse(result)

  def map[A, B](l: List[A], f: A => B): List[B] =
    def mapper (acc: List[B], curr_element: A): List[B] = Cons(f(curr_element), acc)
    val acc = Nil
    val result = foldLeft(l, acc, mapper)
    reverse(result)


  def filter[A](as: List[A], f: A => Boolean): List[A] =
    def filterer(acc: List[A], curr_element: A): List[A] = if f(curr_element) then Cons(curr_element, acc) else acc
    val acc = Nil
    val result = foldLeft(as, acc, filterer)
    reverse(result)

  //We apply f to each element which returns a list, and then we append the lists together
  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    def flatMapper(acc: List[B], curr_element: A): List[B] = appendViaFoldRight(f(curr_element), acc)
    val acc = Nil
    val result = foldLeft(as, acc, flatMapper)
    reverse(result)

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, (a: A) => if f(a) then List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))

  def zipWith[A, B,C](a: List[A], b: List[B], f: (A, B) => C): List[C] =
    @annotation.tailrec
    def loop(a: List[A], b: List[B], acc: List[C]): List[C] =
      (a, b) match
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(f(h1, h2), acc))
    reverse(loop(a, b, Nil))

  def hasSubsequence[A](listToCheck: List[A], seqToFind: List[A]): Boolean =
    @annotation.tailrec
    def loop(listToCheck: List[A], seqToFind: List[A]): Boolean = (listToCheck, seqToFind) match
      case (_, Nil) => true // If seqToFind is empty, we've successfully found the subsequence
      case (Nil, _) => false // If listToCheck is empty but seqToFind is not, the subsequence cannot be found
      case (Cons(h1, t1), Cons(h2, t2)) =>
        // If the heads match, continue checking the rest of both lists
        if h1 == h2 then loop(t1, t2)
        else loop(t1, seqToFind) // Otherwise, move to the next element in listToCheck and try again

    loop(listToCheck, seqToFind)


  @main def testAllMethods: Unit =

    val l = List(1, 2, 3, 4, 5)
    val l2 = List(6, 7, 8, 9)

    // Basic List operations
    assert(sum(l) == 15)
    assert(product(List(1.0, 2.0, 3.0)) == 6.0)
    assert(result == 3)
    assert(sumViaFoldRight(l) == 15)
    assert(productViaFoldRight(List(1.0, 2.0, 3.0)) == 6.0)
    assert(tail(l) == List(2, 3, 4, 5))
    assert(setHead(l, 0) == List(0, 2, 3, 4, 5))
    assert(drop(l, 2) == List(3, 4, 5))
    assert(dropWhile(l, (x: Int) => x < 4) == List(4, 5))
    assert(init(l) == List(1, 2, 3, 4))

    assert(length(l) == 5)
    assert(foldLeft(l, 0, (acc: Int, x: Int) => acc + x) == 15)
    assert(sumViaFoldLeft(l) == 15)
    assert(productViaFoldLeft(List(1.0, 2.0, 3.0)) == 6.0)

    assert(lengthViaFoldLeft(l) == 5)
    assert(reverse(l) == List(5, 4, 3, 2, 1))
    assert(appendViaFoldRight(l, l2) == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    assert(concat(List(l, l2)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    assert(incrementEach(l) == List(2, 3, 4, 5, 6))
    assert(doubleToString(List(1.1, 2.2, 3.3)) == List("1.1", "2.2", "3.3"))

    // Higher-order functions
    assert(map(l, (x: Int) => x * 2) == List(2, 4, 6, 8, 10))
    assert(filter(l, (x: Int) => x % 2 == 0) == List(2, 4))
    assert(flatMap(l, (x: Int) => List(x, x)) == List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5))
    assert(filterViaFlatMap(l, (x: Int) => x % 2 == 0) == List(2, 4))
    assert(addPairwise(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))

    // Additional complex methods
    //assert(hasSubsequence(l, List(1, 2))) // true
    //assert(hasSubsequence(l, List(2, 3, 4))) // true
    assert(!hasSubsequence(l, List(6))) // false

    println("All tests passed!")


