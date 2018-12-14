package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

//  def head[A](l: List[A]): A = l match {
//    case Cons(x, _) => x
//  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => l
    case Cons(_, ls) => ls
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => l
    case Cons(_, ls) => Cons(h, ls)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => l
    case Cons(_, ls) if n > 0 => drop(ls, n-1)
    case _ => l
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => l
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match { // returns all elements except last
    case Nil => l
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(_, xs) => 1 + length(xs)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    //    case Cons(a, as) => foldLeft(as, z)(f)
    case Cons(a, as) => foldLeft(as, f(z, a))(f)
    //    case Cons(a, as) => f(foldLeft(as, z)(f), a)
  }


  def lengthWithFoldRight[A](l: List[A]): Int =
    foldRight(l, 0)((_, x) => x + 1)

  //  3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((xs, x) => Cons(x, xs))

  //  3.13 TODO
  def foldRightWithFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = ???

  //  3.14
  def appendWithFold[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)((a, as) => Cons(a, as))

//  3.15
  def mergeLists[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

//  3.16
  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((x, xs) => Cons(x+1, xs))

//  3.17
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((d, ss) => Cons(d.toString, ss))

//  3.18
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((a, bs) => Cons(f(a), bs))

//  3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((a, bs) => if (f(a)) Cons(a, bs)  else bs)

//  3.20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    mergeLists(map(l)(f))

//  3.21
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if(f(a)) List(a) else Nil)

//  3.22
  def addListsElementwise(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (Nil, _) => r
    case (_, Nil) => l
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addListsElementwise(xs, ys))
  }

//  3.23
  def zipWith[A](l: List[A], r: List[A])(f: (A, A) => A): List[A] = (l, r) match {
    case (Nil, _) => r
    case (_, Nil) => l
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

//  3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def hasSubsequenceFrom(sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(x, xs), Cons(y, ys)) => x == y && hasSubsequenceFrom(xs, ys)
    }

    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(_, xs), _) => hasSubsequenceFrom(sup, sub) || hasSubsequence(xs, sub)
    }
  }

}
