package chapter4

import scala.{ Option => _, Either => _, _ } // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(e) => Right(f(e))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(e) => f(e)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(e) => this
    case Left(e) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def mean(xs: Seq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("we could'nt get the mean of empty lists")
    else Right(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Either[String, Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  // implementation with flatMap and map, see Option for other implementations
  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = a match {
    case Nil => Right(Nil)
    case x :: xs => x flatMap (xx => sequence(xs) map (xx :: _))
  }

  // implementation with for-comprehensions, see Option for other implementations
  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = a match {
    case Nil => Right(Nil)
    case x :: xs => {
      for {
        aa <- f(x)
        bb <- traverse(xs)(f)
      } yield aa :: bb
    }
  }
}