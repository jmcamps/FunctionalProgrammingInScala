package chapter4

import java.util.regex.Pattern
import java.util.regex.PatternSyntaxException

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
  }

  // implementation with flatMap and map combinations
  // is the same as previous example with for comprehensions
  def map3[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  // recursive version
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def go(remaining: List[Option[A]], result: List[A]): Option[List[A]] = remaining match {
      case Nil => Some(result.reverse)
      case None :: xs => None
      case Some(x) :: xs => go(xs, x :: result)
    }
    go(a, Nil)
  }

  // recursive version with flatMap and map
  def sequence2[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case x :: xs => x flatMap (xx => sequence2(xs) map (xx :: _))
  }

  // version with foldRight
  def sequence3[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

  // version with for comprehensions
  def sequence4[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case x :: xs => {
      for {
        aa <- x
        bb <- sequence4(xs)
      } yield aa :: bb
    }
  }

  // trivial implementation with sequence and map
  def traverse1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequence(a map f)

  // recursive version with flatMap and map
  // also we're playing here with partial applied funtions
  def traverse2[A, B](f: A => Option[B])(a: List[A]): Option[List[B]] = {
    val tr = traverse2(f) _
    a match {
      case Nil => Some(Nil)
      case x :: xs => f(x) flatMap (xx => tr(xs) map (xx :: _))
    }
  }

  // version with foldRight
  def traverse3[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((x, y) => map2(f(x), y)(_ :: _))

  // version with for comprehensions 
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case x :: xs => {
      for {
        aa <- f(x)
        bb <- traverse(xs)(f)
      } yield aa :: bb
    }
  }
}