package chapter2

import scala.annotation.tailrec

object Calculator {

  def factorial(n: Int) : Int = {    
    @annotation.tailrec
    def go(n:Int, accumulator: Int) : Int = n match {
      case 0 =>  accumulator
      case _ => go(n-1, n*accumulator)
    }
    go(n, 1)
  }
  
  def fibonnacci(n: Int) : Int = {
    @annotation.tailrec
    def go(n:Int, a:Int, acc:Int): Int = n match {
      case 0 => acc
      case _ => go(n-1, acc, acc + a) 
    }     
    
    go(n, 1, 0)
  }
  
  def isSorted[A](arr: Array[A], gt: (A,A) => Boolean ): Boolean = {
    @annotation.tailrec
    def go(list: List[A], result: Boolean): Boolean = list match {
      case Nil => result
      case x :: Nil => result
      case x :: y :: Nil => gt(x,y)
      case x::y:: ys => if(gt(x,y)) go(y :: ys, true) else false
    }
    go(arr.toList, true)
  } 
  
  def partial1[A, B, C](a: A, f : (A,B) => C) : B => C = (b: B) => f(a, b) 
  
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)
  
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
  
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
}