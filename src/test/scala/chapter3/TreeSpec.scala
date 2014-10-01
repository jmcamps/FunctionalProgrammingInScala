package chapter3

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class TreeSpec extends FlatSpec with ShouldMatchers {

  "size" should "return tree size" in {
    // arrange
    val t = new Branch[Int](new Leaf(1), new Leaf(2))
    val expected = 3

    // act
    val result = Tree.size(t)

    // assert 
    result should equal (expected)
  }
  
  it should "return one for a Leaf" in { 
    // arrange
    val t = new Leaf(1)
    val expected = 1

    // act
    val result = Tree.size(t)

    // assert 
    result should equal (expected)
  }
  
  "maximum" should "return max integer element" in {
    // arrange
    val t = new Branch[Int](new Leaf(1), new Leaf(200))
    val expected = 200

    // act
    val result = Tree.maximum(t)

    // assert 
    result should equal (expected)
  }
  
   "depth" should "return max integer element" in {
    // arrange
    val t = new Branch[Int](new Leaf(1), new Branch(new Leaf(200), new Leaf(300)))
    val expected = 3

    // act
    val result = Tree.depth(t)

    // assert 
    result should equal (expected)
  }
   
    "map" should "add 1 to all elements in tree with add one function" in {
    // arrange
    val l = new Branch[Int](Leaf(1), Branch(Leaf(200), Leaf(300)))
    val expected = new Branch[Int](Leaf(2), Branch(Leaf(201), Leaf(301)))

    // act
    val result = Tree.map(l)(x => x+1)
   
    // assert 
    result should equal (expected)
  }
    
    "size via fold" should "return tree size" in {
    // arrange
    val t = new Branch[Int](new Leaf(1), new Leaf(2))
    val expected = 3

    // act
    val result = Tree.foldSize(t)

    // assert 
    result should equal (expected)
  }
  
  it should "return one for a Leaf" in { 
    // arrange
    val t = new Leaf(1)
    val expected = 1

    // act
    val result = Tree.foldSize(t)

    // assert 
    result should equal (expected)
  }
  
  "maximum via fold" should "return max integer element" in {
    // arrange
    val t = new Branch[Int](new Leaf(1), new Leaf(200))
    val expected = 200

    // act
    val result = Tree.foldMaximum(t)

    // assert 
    result should equal (expected)
  }
  
   "depth via fold" should "return max integer element" in {
    // arrange
    val t = new Branch[Int](new Leaf(1), new Branch(new Leaf(200), new Leaf(300)))
    val expected = 3

    // act
    val result = Tree.foldDepth(t)

    // assert 
    result should equal (expected)
  }
   
    "map via fold" should "add 1 to all elements in tree with add one function" in {
    // arrange
    val l = new Branch[Int](Leaf(1), Branch(Leaf(200), Leaf(300)))
    val expected = new Branch[Int](Leaf(2), Branch(Leaf(201), Leaf(301)))

    // act
    val result = Tree.foldMap(l)(x => x+1)
   
    // assert 
    result should equal (expected)
  }

}