package chapter4;

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest._
import Matchers._

@RunWith(classOf[JUnitRunner])
class PersonMakerSpec extends FlatSpec {

  "Factory" should "create a person if all inputs are valid" in {
    // arrange
   
    // act
    val result =  PersonMaker("xampi", 41)
    
    // assert
    result should be (Right(Person(Name("xampi"), Age(41)))) 
  } 
  
  it should "report error in name for empty input strings" in {
    // arrange
   
    // act
    val result =  PersonMaker("", 41)
    
    // assert
    result should be (Left("Name is empty.")) 
  } 
  
  it should "report error in age for ages < 0" in {
    // arrange
   
    // act
    val result =  PersonMaker("xampi", -1)
    
    // assert
    result should be (Left("Age is out of range.")) 
  } 
  
//  it should "is able to report two errors, if both the name and the age are invalid" in {
//    // arrange
//   
//    // act
//    val result =  PersonMaker("", -1)
//    
//    // assert
//    result should be (Left("Name is empty.", "Age is out of range.")) 
//  } 
  
  
}