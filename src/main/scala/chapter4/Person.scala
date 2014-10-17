package chapter4

sealed trait Partial[+A,+B] { 
  def map[C](f: B => C): Partial[A, C] = this match {
    case Success(e) => Success(f(e))
    case Errors(e) => Errors(e)
  }   
}


case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
case class Success[+B](get: B) extends Partial[Nothing,B]



case class Person(name: Name, age: Age)
sealed case class Name(val value: String)
sealed case class Age(val value: Int)

object PersonMaker {
  private def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  private def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  private def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))
    
  def apply(name: String, age: Int) = mkPerson(name, age)
}