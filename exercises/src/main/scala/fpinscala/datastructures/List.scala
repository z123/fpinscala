package fpinscala.datastructures

import scala.annotation.tailrec

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


  def tail[A](l: List[A]): List[A] = l match { case Cons(_, xs) => xs }

  def setHead[A](l: List[A], h: A): List[A] = l match { case Cons(_, xs) => Cons(h, xs)}

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => drop(tail(l), n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    def helper[A](l: List[A]) = l match { case Cons(h, t) => h }
    f(helper(l)) match {
      case true => dropWhile(tail(l), f)
      case false => l
    }
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((x, y) => 1 + y)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  /*
    Cons(1, Cons(2, Cons(3, nil)))
    Cons(4, Cons(5, Cons(6, nil)))

   */
  def appendViaFoldRight[A](a1: List[A], a2: List[A]) = foldRight(a1, a2)((h, acc) => Cons(h, acc))
  def appendViaFoldLeft[A](a1: List[A], a2: List[A]) = foldLeft(a1, a2)((acc, h) => Cons(h, acc))

  def reverse[A](l: List[A]) = foldRight(l, Nil: List[A])((x: A, y: List[A]) => Cons(x, y))

  def concat[A](l: List[List[A]]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => append(x, concat(xs))
  }

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    def helper(g: (B, A) => B): (A, B) => B = (A, B) => g(B, A)
    foldRight(l, z)(helper(f))
  }

  def foldRight2[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    def helper(g: (B, A) => B): (A, B) => B = (A, B) => g(B, A)
    foldRight(l, z)(helper(f))
  }

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def product3(l: List[Int]): Int = foldLeft(l, 1)(_ * _)
  def length2(l: List[Int]): Int = foldLeft(l, 0)((x, y) => 1 + y)

  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}
