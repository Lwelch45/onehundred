/**
  * Created by johnmontroy on 1/25/17.
  */
object onehundred extends App {

  /* P01: Find the last element of a list.
    Example:
    scala> last(List(1, 1, 2, 3, 5, 8))
    res0: Int = 8 */

  // originally got it wrong because i misordered the case statements - Nil is an elem so if you x :: xs first, oops
  def last[A](lst: List[A]): A = lst match {
    case x :: Nil => x
    case x :: xs => last(xs)
    case _ => throw new NoSuchElementException
  }

  /* P02: Find the last but one element of a list.
    Example:
    scala> penultimate(List(1, 1, 2, 3, 5, 8))
    res0: Int = 5
  */

  // after first one, easy
  def penultimate[A](lst: List[A]): A = lst match {
    case x :: y :: Nil => x
    case x :: xs => penultimate(xs)
    case _ => throw new NoSuchElementException
  }

  /* P03: Find the Kth element of a list.
    By convention, the first element in the list is element 0.
    Example:

    scala> nth(2, List(1, 1, 2, 3, 5, 8))
    res0: Int = 2
  */

  // case class allows "filtered" unpack, nice
  def nth[A](n: Int, lst: List[A]): A = lst match {
    case x :: xs if n == 0 => x
    case x :: xs => nth(n - 1, xs)
    case _ => throw new NoSuchElementException
  }

  // given: tuple match! i dig this. also, don't give an elem a symbol if you don't use it.
  /* def nthRecursive[A](n: Int, ls: List[A]): A = (n, ls) match {
    case (0, h :: _   ) => h
    case (n, _ :: tail) => nthRecursive(n - 1, tail)
    case (_, Nil      ) => throw new NoSuchElementException
  } */

  /* P04: Find the number of elements of a list.
    Example:
    scala> length(List(1, 1, 2, 3, 5, 8))
    res0: Int = 6
  */

  def length[A](lst: List[A]): Int = {
    def lengthAccum[A](ln: Int, lstAccum: List[A]): Int = lstAccum match {
      case x :: Nil => ln + 1
      case x :: xs => lengthAccum(ln + 1, xs)
      case _ => throw new NoSuchElementException
    }
    lengthAccum(0, lst)
  }

  /* // More pure functional solution, with folds.
  def lengthFunctional[A](ls: List[A]): Int = ls.foldLeft(0) { (c, _) => c + 1 } */
  // JM: remember the first "c" above is the base case defined pre-curry (or whatevs)

  /* P05: Reverse a list.
      Example:
      scala> reverse(List(1, 1, 2, 3, 5, 8))
      res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  */

  // KISS: a small addition in space (and not even really depending on how efficient the immutable pointer swapping
  // blah-blah is) improves on time and makes your code a lot simpler to understand
  def reverse[A](lst: List[A]): List[A] = {
    def reverseAccum[A](lstBase: List[A], lstRev: List[A]): List[A] = lstBase match {
      case x :: Nil => x :: lstRev
      case x :: xs => reverseAccum(xs, x :: lstRev)
      case _ => throw new NoSuchElementException
    }
    reverseAccum(lst, List())
  }

  /* // Pure functional
  def reverseFunctional[A](ls: List[A]): List[A] =
    ls.foldLeft(List[A]()) { (r, h) => h :: r } */
  // scrooging: (1 to 10).toList.foldLeft(List[Int]())((res, elm) => elm :: res)
  // this: element to element vs "structure" (head/tail in this case, but don't get caught up on that)


  /* P06: Find out whether a list is a palindrome.
  Example:
    scala> isPalindrome(List(1, 2, 3, 2, 1))
  res0: Boolean = true */

  def isPalindrome[A](lst: List[A]): Boolean = {
    def isPalInner[A](l1: List[A], l2: List[A]): Boolean = (l1, l2) match {
      case (l1 :: Nil, l2 :: Nil) if (l1 == l2) => true
      case (l1 :: l1t, l2 :: l2t) if (l1 == l2) => isPalInner(l1t, l2t)
      case _ => false
    }
    isPalInner(lst, reverse(lst))
  }

  /* P07: Flatten a nested list structure.
    Example:
    scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    res0: List[Any] = List(1, 1, 2, 3, 5, 8)
  */

  // not my solution...well if i knew flatMap was an option!!
  def flatten(lst: List[Any]): List[Any] = lst flatMap {
    case lst: List[_] => flatten(lst)
    case elm => List(elm)
  }

  /* P08: Eliminate consecutive duplicates of list elements.
    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
    scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
  */

  // this risks a stack overflow obvs, tail-recursive would be with an inner function accumulator
  def compress[T](lst: List[T]): List[T] = lst match {
    case x :: Nil => List(x)
    case x :: y :: xs if x == y => compress(x :: xs)
    case x :: xs => List(x) ++ compress(xs)
    case _ => throw new NoSuchElementException
  }

  /* P09: Pack consecutive duplicates of list elements into sublists.
    If a list contains repeated elements they should be placed in separate sublists.
    Example:

    scala>
    res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  */

  def pack[T](lst: List[T]): List[List[T]] = lst match {
    case Nil => Nil
    case h :: t => {
      val (spanned, rest) = lst.span(elm => elm == h)
      spanned :: pack(rest)
    }
  }

  /* P10: Run-length encoding of a list.
      Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
      Example:

      scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)) */

  def encode[T](lst: List[T]): List[(Int,T)] = {
    pack(lst).map(sublist => (sublist.size,sublist.head))
  }

  /* P11: Modified run-length encoding.
    Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
    Example:

    scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)) */

  def encodeModified[T](lst: List[T]): List[Any] = {
    encode(lst).map {
      case (n,elm) if n == 1 => elm
      case a => a
    }
  }

  /* P12: Decode a run-length encoded list.
      Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
      Example:

      scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
      res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e) */

  def decode[T](lst: List[(Int,T)]): List[Any] = {
    lst.flatMap(enc => { for {i <- 0 until enc._1} yield enc._2})
  }


  /* P13: Run-length encoding of a list (direct solution).
    Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
    Example:

    scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)) */

  def encodeDirect[T](lst: List[T]): List[(Int,T)] = {
    def encodeDirectIter[T](lst: List[T], cnt: Int, elm: T): List[(Int,T)] = {
      lst match {
        case Nil => Nil
        case h :: t if h == elm => encodeDirectIter(t, cnt + 1, elm)
        case h :: t => (cnt,elm) :: encodeDirectIter(t, 1, h)
      }
    }
    encodeDirectIter(lst.tail,1,lst.head)
  }

}


object utils {
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns\n")
    result
  }
}
