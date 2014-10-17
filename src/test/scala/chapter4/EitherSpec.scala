package chapter4;

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest._
import Matchers._

@RunWith(classOf[JUnitRunner])
class EitherSpec extends FlatSpec {

  "mean" should "return Right(mean) for non empty sequences" in {
    // arrange
    val input: Seq[Double] = Seq(1,2,3)
    
    // act
    val result =  Either.mean(input)  
    
    // assert
    result should be (Right(2)) 
  } 
  
  it should "return Left for empty sequences" in {
    // arrange
    val input: Seq[Double] = Seq()
    
    // act
    val result =  Either.mean(input)  
    
    // assert
    result should be (Left("we could'nt get the mean of empty lists")) 
  }
  
  "variance" should "return Right(variance) for non empty sequences" in {
    // arrange
    val input: Seq[Double] = Seq(1,2,3)
    
    // act
    val result =  Either.variance(input)  
    
    // assert
    result should be (Right(0.6666666666666666)) 
  } 
  
  it should "return Left for empty sequences" in {
    // arrange
    val input: Seq[Double] = Seq()
    
    // act
    val result =  Either.variance(input)  
    
    // assert
    result should be (Left("we could'nt get the mean of empty lists")) 
  }
  
  "map2" should "return Right(c) if f returns any value" in {
    // arrange   
    
    // act
    val result =  Right(1).map2(Right(2))((x,y)=> 1)  
    
    // assert
    result should be (Right(1)) 
  } 
  
  it should "return None if a is None" in {
    // arrange
    def f(a: Int, b:Int) = 1
    
    // act
    val result =  Left("invalid").map2(Right(2))(f)  
    
    // assert
    result should be (Left("invalid")) 
  } 
  
  it should "return None if b is None" in {
    // arrange
    def f(a: Int, b:Int) = 1
    
    // act
    val result =  Right(1).map2(Left("invalid"))(f)  
    
    // assert
    result should be (Left("invalid")) 
  } 
  
  "sequence" should "return Right with a sequence containing all values if all values are Right()" in {
    // arrange   
    val input = List(Right(1), Right(2))
    // act
    val result =  Either.sequence(input)  
    
    // assert
    result should be (Right(List(1,2))) 
  } 
  
  it should "return Left with a sequence containing any None value" in {
    // arrange   
    val input = List(Right(1), Right(2), Left("invalid"), Right(4))
    // act
    val result =  Either.sequence(input)  
    
    // assert
    result should be (Left("invalid")) 
  } 

  "traverse" should "return Right with a sequence containing all function applications if all function applications are Rigth()" in {
    // arrange   
    val input = List(1,2)    
    // act
    val result =  Either.traverse(input)(x => Right(x*x)) 
    
    // assert
    result should be (Right(List(1,4))) 
  } 
  
  it should "return Left if almost one function application are Left" in {
    // arrange   
    val input = List(1,2,-1)    
    def f(x: Int) = x match {
      case value => if(value>0) Right(value) else Left("invalid")
    }
    // act
    val result =  Either.traverse(input)(f) 
    
    // assert
    result should be (Left("invalid")) 
  } 
  
}