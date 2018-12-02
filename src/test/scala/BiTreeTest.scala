import org.scalatest.FlatSpec
import variance.hw.{BiTree, Empty}

//https://www.cs.cmu.edu/~adamchik/15-121/lectures/Trees/trees.html

class BiTreeTest extends FlatSpec {
  "Empty tree" should  "add value" in {
    val emptyTree = Empty
    val res = emptyTree.add(45)
    assert(res.contains(45))
  }

  it should "throw exception while access to value" in {
    assertThrows[IllegalAccessException] { // Result type: Assertion
      Empty.value
    }
  }

  it should "throw exception while access to left node" in {
    assertThrows[IllegalAccessException] { // Result type: Assertion
      Empty.left
    }
  }

  it should "throw exception while access to right node" in {
    assertThrows[IllegalAccessException] { // Result type: Assertion
      Empty.right
    }
  }

  it should "not contain elements" in {
    assert(Empty.contains(1) == false)
  }

  "Binary tree" should "contains added elements" in {
   val tree = generateIntTree(List(11, 6, 8, 19, 4, 10, 5, 17, 43, 49, 31))
    assert(tree.contains(43))
    assert(tree.contains(17))
  }

  it should "not contain not added elements" in {
    val tree = generateIntTree(List(11, 6, 8, 19, 4, 10, 5, 17, 43, 49, 31))
    assert(tree.contains(1) === false)
    assert(tree.contains(15) === false)
  }

  it should "work with big trees fine" in {
    import util.Random.nextInt
    val ls = Stream.continually(nextInt(100)).take(1000000).toList
    val tree = generateIntTree(ls)
    for (e <- ls) {
      assert(tree.contains(e))
    }
  }

  it should "work contrvariant types" in {
    import variance.hw._
    val ls = List (new Dog("Alf"), new Dog("Bobik"), new Dog("Dog"), new Pet ("Sharik"), new Pet("Alf"))
    val tree =  ls.foldLeft[BiTree[Animal]](Empty)((tr, el) ⇒ tr.add(el))
    for (e <- ls) {
      assert(tree.contains(e))
    }
  }

  def generateIntTree(l: List[Int]): BiTree[Int] = {
    l.foldLeft[BiTree[Int]](Empty)((tr, el) ⇒ tr.add(el))
  }
}

