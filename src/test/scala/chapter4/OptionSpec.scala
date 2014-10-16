package chapter4;

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest._
import Matchers._

@RunWith(classOf[JUnitRunner])
class OptionSpec extends FlatSpec {

  "mean" should "return Some(mean) for non empty sequences" in {
    // arrange
    val input: Seq[Double] = Seq(1,2,3)
    
    // act
    val result =  Option.mean(input)  
    
    // assert
    result should be (Some(2)) 
  } 
  
  it should "return None for empty sequences" in {
    // arrange
    val input: Seq[Double] = Seq()
    
    // act
    val result =  Option.mean(input)  
    
    // assert
    result should be (None) 
  }
  
  "variance" should "return Some(variance) for non empty sequences" in {
    // arrange
    val input: Seq[Double] = Seq(1,2,3)
    
    // act
    val result =  Option.variance(input)  
    
    // assert
    result should be (Some(0.6666666666666666)) 
  } 
  
  it should "return None for empty sequences" in {
    // arrange
    val input: Seq[Double] = Seq()
    
    // act
    val result =  Option.variance(input)  
    
    // assert
    result should be (None) 
  }
  
  "map2" should "return Some(c) if f returns any value" in {
    // arrange   
    
    // act
    val result =  Option.map2(Some(1), Some(2))((x,y)=> 1)  
    
    // assert
    result should be (Some(1)) 
  } 
  
  it should "return None if a is None" in {
    // arrange
    def f(a: Int, b:Int) = 1
    
    // act
    val result =  Option.map2(None, Some(2))(f)  
    
    // assert
    result should be (None) 
  } 
  
  it should "return None if b is None" in {
    // arrange
    def f(a: Int, b:Int) = 1
    
    // act
    val result =  Option.map2(Some(1), None)(f)  
    
    // assert
    result should be (None) 
  } 
  
  "sequence" should "return Some with a sequence containing all values if all values are Some()" in {
    // arrange   
    val input = List(Some(1), Some(2))
    // act
    val result =  Option.sequence(input)  
    
    // assert
    result should be (Some(List(1,2))) 
  } 
  
  it should "return None with a sequence containing any None value" in {
    // arrange   
    val input = List(Some(1), Some(2), None, Some(4))
    // act
    val result =  Option.sequence(input)  
    
    // assert
    result should be (None) 
  } 

  "traverse" should "return Some with a sequence containing all function applications if all function applications are Some()" in {
    // arrange   
    val input = List(1,2)    
    // act
    val result =  Option.traverse(input)(x => Some(x*x)) 
    
    // assert
    result should be (Some(List(1,4))) 
  } 
  
  it should "return None if almost one function application are None" in {
    // arrange   
    val input = List(1,2,-1)    
    def f(x: Int) = x match {
      case value => if(value>0) Some(value) else None
    }
    // act
    val result =  Option.traverse(input)(f) 
    
    // assert
    result should be (None) 
  } 
  
  "traverse2" should "return Some with a sequence containing all function applications if all function applications are Some()" in {
    // arrange   
    val input = List(1,2)    
    // act
    val result =  Option.traverse2((x:Int) => Some(x*x))(input) 
    
    // assert
    result should be (Some(List(1,4))) 
  } 
  
  it should "return None if almost one function application are None" in {
    // arrange   
    val input = List(1,2,-1)    
    def f(x: Int) = x match {
      case value => if(value>0) Some(value) else None
    }
    // act
    val result =  Option.traverse2(f)(input) 
    
    // assert
    result should be (None) 
  } 
}