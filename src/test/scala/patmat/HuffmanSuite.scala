package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val singletonTreeList = combine(leaflist)
    assert(singletonTreeList.length < 2)
    assert(combine(leaflist) === List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Leaf('x',4),List('e', 't', 'x'),7)))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  // times
  test("check times for value count") {
    val f = List('a', 'b', 'c', 'a', 'a', 'e', 'c')
    val aList: List[(Char, Int)] = times(f)
    assert(aList.contains(('a',3)))
    assert(aList.contains(('b',1)))
    assert(aList.contains(('c',2)))
    assert(aList.contains(('e',1)))
  }

  // singleton
  test("check for singleton tree") {
    assert(singleton(List[CodeTree]()))
    assert(!singleton(List[CodeTree](Leaf('a', 3), Leaf('b', 4))))
    assert(singleton(List[CodeTree](Fork(Leaf('a', 3), Leaf('b', 3), List('a','b'), 3))))
  }

  test("check decoded message") {
    assert(decode(frenchCode, secret) === List('h','u','f','f','m','a','n','e','s','t','c','o','o','l'))
  }

  test("check encoded secret") {
    val message = List('h','u','f','f','m','a','n','e','s','t','c','o','o','l')
    def frenchEncoder(message: List[Char]) = encode(frenchCode) _
    assert(encode(frenchCode)(message)  === secret)
    }

  test("decode and encode with quickEncode a very short text should be identity") {
    new TestTrees {
      assert(decode(t2, quickEncode(t2)("ab".toList)) === "ab".toList)
    }
  }

  test ("decode and encode with quickEncode a text should be identity") {
    new TestTrees {
      assert(decode(t2, quickEncode(t2)("abbbb".toList)) === "abbbb".toList)
    }
  }

  test ("decode and encode with quickEncode a super long code") {
    new TestTrees {
      val longText = """We'll start by revisiting some concepts that we have learned from Principles of Functional Programming in Scala; collections, pattern matching, and functions. We'll then touch on for-comprehensions, a powerful way in Scala to traverse a list, process it, and return a new list. We'll see how to do queries with for-comprehensions as well as how the for-comprehension is "desugared" into calls to higher-order functions by the Scala compiler. Finally, we'll discuss what monads are, and how to verify that the monad laws are satisfied for a number of examples."""
      val codeTree = createCodeTree(longText.toList)
      assert(decode(codeTree, quickEncode(codeTree)(longText.toList)) === longText.toList)
    }
  }
}
