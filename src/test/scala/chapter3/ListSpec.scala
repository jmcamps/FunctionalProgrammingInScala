package chapter3

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class ListSpec extends FlatSpec with ShouldMatchers {

  "Tail" should "throw an exception on empty list" in {
    // arrange
    val l = Nil
    val expected = Nil

    // act
    val thrown = intercept[RuntimeException] {
      val result = List.tail(l)
    }

    // assert 
    assert(thrown.getMessage === "tail of empty list")
  }
  
  it should "return Nil on a list with one element" in {
    // arrange
    val l = List(1)
    val expected = Nil

    // act
    val result = List.tail(l)
   
    // assert 
    result should equal (expected)
  }
  
  it should "delete first element on a list with two elements" in {
    // arrange
    val l = List(1,2)
    val expected = List(2)

    // act
    val result = List.tail(l)
   
    // assert 
    result should equal (expected)
  }
  
  it should "delete first element on a list with four elements" in {
    // arrange
    val l = List(1,2,3,4)
    val expected = List(2,3,4)

    // act
    val result = List.tail(l)
   
    // assert 
    result should equal (expected)
  }
  
   "Drop" should "throw an exception on empty list" in {
    // arrange
    val l = Nil
    
    // act
    val thrown = intercept[RuntimeException] {
      val result = List.drop(1,l)
    }

    // assert 
    assert(thrown.getMessage === "list with insuficient elements")
  }
   
   it should "throw an exception on insuficient elements in list" in {
    // arrange
    val l = List(1,2,3,4)
    
    // act
    val thrown = intercept[RuntimeException] {
      val result = List.drop(5,l)
    }

    // assert 
    assert(thrown.getMessage === "list with insuficient elements")
  }
   
   it should "delete all elements in list" in {
    // arrange
    val l = List(1,2,3,4)
    val expected = Nil

    // act
    val result = List.drop(4,l)
   
    // assert 
    result should equal (expected)
  }
   
   it should "delete four first elements in list" in {
    // arrange
    val l = List(1,2,3,4,5)
    val expected = List(5)

    // act
    val result = List.drop(4,l)
   
    // assert 
    result should equal (expected)
  }
   
   "DropWhile" should "not delete any element if predicate doesn't match" in {
    // arrange
    val l = List(1)
    val expected = List(1)

    // act
    val result = List.dropWhile(l)(x => x < 0)
   
    // assert 
    result should equal (expected)
  }
   
   it should "delete all elements if predicate match for all" in {
    // arrange
    val l = List(1,2,3,4)
    val expected =Nil

    // act
    val result = List.dropWhile(l)(x =>x > 0)
   
    // assert 
    result should equal (expected)
  }
   
   it should "delete elements until predicate does'nt match" in {
    // arrange
    val l = List(1,2,-3,4)
    val expected =List(-3,4)

    // act
    val result = List.dropWhile(l)(x => x > 0)
   
    // assert 
    result should equal (expected)
  }
   
   "foldRight" should "sum all elements in list" in {
    // arrange
    val l = List(1,2,3)
    val expected = 6

    // act
    val result = List.foldRight(l, 0)(_ + _)
   
    // assert 
    result should equal (expected)
  }
   
   it should "multiply all elements in list" in {
    // arrange
    val l = List(1,2,3,4)
    val expected = 24

    // act
    val result = List.foldRight(l, 1.0)(_ * _)
   
    // assert 
    result should equal (expected)
  }
   
   it should "return the original list" in {
    // arrange
    val l = List(1,2,3,4)
    val expected = List(1,2,3,4)

    // act
    val result = List.foldRight(l, Nil:List[Int])(Cons(_,_))
   
    // assert 
    result should equal (expected)
  }
   
   it should "compute length" in {
    // arrange
    val l = List(1,2,3,4)
    val expected = 4

    // act
    val result = List.foldRight(l, 0)((_,acc) => acc + 1)
   
    // assert 
    result should equal (expected)
  }
   
   "foldLeft" should "sum all elements in list" in {
    // arrange
    val l = List(1,2,3)
    val expected = 6

    // act
    val result = List.foldLeft(l, 0)(_ + _)
   
    // assert 
    result should equal (expected)
  }
   
   it should "multiply all elements in list" in {
    // arrange
    val l = List(1,2,3,4)
    val expected = 24

    // act
    val result = List.foldLeft(l, 1.0)(_ * _)
   
    // assert 
    result should equal (expected)
  }
   
    "reverse" should "reverse all elements in list" in {
    // arrange
    val l = List(1,2,3)
    val expected = List(3,2,1)

    // act
    val result = List.reverse(l)
   
    // assert 
    result should equal (expected)
  }
    
     "map" should "add 1 to all elements in list with add one function" in {
    // arrange
    val l = List(1,2,3)
    val expected = List(2,3,4)

    // act
    val result = List.map(l)(x => x+1)
   
    // assert 
    result should equal (expected)
  }
   
     "filter" should "remove all odd elements" in {
    // arrange
    val l = List(1,2,3)
    val expected = List(2)

    // act
    val result = List.filter(l)(x => x%2 == 0)
   
    // assert 
    result should equal (expected)
  }

   
   

}