package fpinscala.datastructures
import scala.math.max

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

//  3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

//3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => max(maximum(l), maximum(r))
  }

//  3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + max(depth(l), depth(r))
  }

//  3.28
  def map[A, B](t: Tree[A])( f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

//  3.29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)


  def maxViaFold(t: Tree[Int]): Int =
    fold(t)(x => x)(max)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)(1 + max(_, _))

  def mapViaFold[A, B](t: Tree[A])(f: A => B) =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}