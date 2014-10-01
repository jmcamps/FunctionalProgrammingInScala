package chapter2

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class CalculatorSpec extends FlatSpec with ShouldMatchers{    
  
	"Factorial" should "return 1 for factorial(0)" in {
	  // arrange	 
	  val expected = 1
	  
	  // act
	  val result = Calculator.factorial(0)
	  
	  // assert
	  result should equal (expected)	  
	}
	
	it should "return 1 for factorial(1)" in {
	  // arrange	  
	  val expected = 1
	  
	  // act
	  val result = Calculator.factorial(1)
	  
	  // assert
	  result should equal (expected)
	}
	
	it should "return 2 for factorial(2)" in {
	  // arrange	  
	  val expected = 2
	  
	  // act
	  val result = Calculator.factorial(2)
	  
	  // assert
	  result should equal (expected)
	}
	
	it should "return 6 for factorial(3)" in {
	  // arrange	 
	  val expected = 6
	  
	  // act
	  val result = Calculator.factorial(3)
	  
	  // assert
	  result should equal (expected)
	}
	
	"Fibonnacci" should "return 0 for fibonnacci(0)" in {
	  // arrange
	  val expected =0
	  
	  // act
	  val result = Calculator.fibonnacci(0)
	  
	  // assert
	  result should equal (expected)	  
	}
	
	it should "return 1 for fibonnacci(1)" in {
	  // arrange
	  val expected = 1
	  
	  // act
	  val result = Calculator.fibonnacci(1)
	  
	  // assert
	  result should equal (expected)
	}
	
	it should "return 1 for fibonnacci(2)" in {
	  // arrange
	  val expected = 1 
	  
	  // act
	  val result = Calculator.fibonnacci(2)
	  
	  // assert
	  result should equal (expected)
	}
	
	it should "return 987 for fibonnacci(16)" in {
	  // arrange
	  val expected = 987
	  
	  // act
	  val result = Calculator.fibonnacci(16)
	  
	  // assert
	  result should equal (expected)
	}
	
	"isSorted" should "return true for Array(1) and gt" in {
	  // arrange
	  val input = Array(1)
	  val expected = true
	  
	  // act
	  val result = Calculator.isSorted(input, (a: Int,b:Int) => a > b)
	  
	  // assert
	  result should equal (expected)	   
	}
	
	"it" should "return true for Array(2,1) and gt" in {
	  // arrange
	  val input = Array(2,1)
	  val expected = true
	  
	  // act
	  val result = Calculator.isSorted(input, (a: Int,b:Int) => a > b)
	  
	  // assert
	  result should equal (expected)	   
	}
	
	"it" should "return false for Array(2,1,3) and gt" in {
	  // arrange
	  val input = Array(2,1,3)
	  val expected = false
	  
	  // act
	  val result = Calculator.isSorted(input, (a: Int,b:Int) => a > b)
	  
	  // assert
	  result should equal (expected)	   
	}
}