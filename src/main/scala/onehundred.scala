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

    scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  */

  // hitting the limits of pattern matching pretty hard here
  // hrmmm...the real solution just uses span which makes it easy. how would i handle this stacking of Lists?
  def pack[T](lst: List[T]): List[Any] = lst match {
      case Nil => lst
      case (x: List[T]) :: (xs: List[T]) :: Nil if xs.isEmpty => x
      case (x: T) :: (y: T) :: xs if x == y => {println(lst, "here1"); pack(List(List(x,y), xs)) }
      case (x: List[T]) :: (xs: List[T]) :: _ if x.last == xs.head => {println(lst, "here2");pack(List(x ++ List(xs.head), xs.tail))}
      case (x: List[T]) :: (xs: List[T]) :: _ => {println(lst, "here3");List(x, pack(xs))}
      case (x: T) :: (xs: List[T]) :: _ => {println(lst, "here4");List(List(x), pack(xs))}
      case (x: T) :: (y: T) :: xs => {println(lst, "here5"); List(List(x), pack(y :: xs)) }
      case _ => throw new NoSuchFieldException()
    }

  }

  /* ---------------------- */
  val lst: List[Int] = 1 until 10 toList
  val lst2: List[List[Int]] = List(1 to 10 toList, 11 to 20 toList)
  val lst_compress: List[Char] = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e')

//  utils.time { println(last(lst)) }
//  utils.time { println(penultimate(lst)) }
//  utils.time { println(nth(3, lst)) }
//  utils.time { println(length(lst)) }
//  utils.time { println(reverse(lst))}
//  utils.time { println(isPalindrome(List(1,2,3,2,1)))}
//  utils.time { println(flatten(lst2)) }
//  utils.time { println(compress(lst_compress)) }
  utils.time { println(pack(lst_compress)) }

}


object utils {
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns\n")
    result
  }

  // Runtime.getRuntime().totalMemory();

}
