package chapter3

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](ds: List[A]): List[A] = ds match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, xs) => xs
  }

  def drop[A](n: Int, ds: List[A]): List[A] =
    if (n <= 0) ds
    else ds match {
      case Nil => sys.error("list with insuficient elements")
      case Cons(_, xs) => drop(n - 1, xs)
    }

  def dropWhile[A](ds: List[A])(f: A => Boolean): List[A] = ds match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => ds
  }

  def foldRight[A, B](ds: List[A], z: B)(f: (A, B) => B): B = ds match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  @annotation.tailrec
  def foldLeft[A, B](ds: List[A], z: B)(f: (A, B) => B): B = ds match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(x, z))(f)
  }

  def reverse[A](ds: List[A]): List[A] = {
    List.foldLeft(ds, List[A]())((h, acc) => Cons(h, acc))
  }

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((x, xs) => Cons(f(x), xs))

  def filter[A, B](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)

  
}