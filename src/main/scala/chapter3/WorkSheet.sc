package chapter3

object WorkSheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
 
  
  val x = List(1,2,3)                             //> x  : chapter3.List[Int] = Cons(1,Cons(2,Cons(3,Nil)))
  
  val f = (x:Int) => x < 0                        //> f  : Int => Boolean = <function1>
  }