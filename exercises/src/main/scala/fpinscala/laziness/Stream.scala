package fpinscala.laziness

import Stream._

trait Stream[+A] {

  //  5.1
  def toListRecursive: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListRecursive
  }

  def toList: List[A] = {
    @annotation.tailrec
    def toReverseList(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => toReverseList(t(), h() :: acc)
    }
    toReverseList(this, Nil).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  //  5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t() take n-1)
    case _ => Empty
  }
  //  {
  //    if (n <= 0) Empty
  //    else this match {
  //      case Empty => Empty
  //      case Cons(h, t) => cons(h(), t().take(n-1))
  //    }
  //  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t() drop(n-1)
    case _ => this
  }
  //  {
  //    if (n <= 0) this
  //    else this match {
  //      case Empty => Empty
  //      case Cons(_, t) => t().drop(n-1)
  //    }
  //  }

  //  5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => Empty
  }

  //  5.5
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else Empty)

  //  5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  //  5.6
  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a, b) => if(p(a)) cons(a, b) else b)

  def append[B>:A](s: Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((a, b) => f(a) append b)

//  5.14 returns true if this starts with s
//  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
//    case (_, Empty) => true
//    case (Empty, _) => false
//    case (Cons(h1, t1), Cons(h2, t2)) => h1 == h2 && t1().startsWith(t2())
//  }

  def startsWith[B](s: Stream[B]): Boolean = {
    zipAll(s) takeWhile(_._2.isEmpty) forAll {
      case (h1, h2) => h1 == h2
    }
  }

  def tails: Stream[Stream[A]] =
    unfold(this){
      case s@Cons(_, t) => Some((s, t()))
      case _ => None
    } .append(Stream(Empty))

  //  5.13 Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and zipAll
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)){
      case (Empty, _) => None
      case (_, 0) => None
      case (Cons(h, t), m) => Some(h(), (t(), m-1))
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this){
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](t: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, t)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(sh, st), Cons(th, tt)) => Some((f(sh(), th()), (st(), tt())))
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (_, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
      case(Cons(h, t), _) => Some((Some(h()), None), (t(), Empty))
      case _ => None

    }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  //  5.8
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  //  5.9
  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  //  5.10
  def fibs: Stream[Int] = {
    def go(curr: Int, next: Int): Stream[Int] =
      cons(curr, go(next, curr + next))
    go(0, 1)
  }

  //  5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  //  {
  //    f(z).fold(empty: Stream[A])((a: A, s: S): Stream[A] => cons(a, unfold(s)(f)))
  //    f(z).map((a: A, s: S): Stream[A] => cons(a, unfold(s)(f)))
  //  }
    f(z) match {
      case None => empty
      case Some((value, nextState)) => cons(value, unfold(nextState)(f))
    }

  //  5.12 Write fibs, from, constant, and ones in terms of unfold
  def onesViaUnfold: Stream[Int] =
    unfold(1)(_ => Some(1, 1))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)( _ => Some(a, a))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(m => Some(m, m+1))

  def fibsViaUnfold: Stream[Int] = {
    unfold((0, 1)) { case (curr, next) => Some(curr, (next, curr + next)) }
  }

}