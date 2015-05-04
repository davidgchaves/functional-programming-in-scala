/*
 * Load Chapter3Module.scala on the REPL by simply:
 *  scala> :load Chapter3Module.scala
 * Then it's useful to import everything in the module with:
 *  scala> import Chapter3Module
 */

object Chapter3Module {

  /*
   * The Linked List Algebraic Data Type (ADT) parameterized on type A, with 2 data constructors:
   *  - Nil               (we can construct an empty list with Nil)
   *  - Cons(head,tail)   (we can construct a non-empty list with Cons(head,tail))
   * We can construct types and also pattern match against them
   */
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  /*
   * NOTES:
   *  - sealed means that all implementations of the trait must be declared in this file
   *  - not only functions can be parametric, data types can be as well [+A]
   *  - COVARIANCE 101 for List[+A]
   *    * [+A] -> A is a covariant or "positive" parameter of List
   *    * For all types X and Y, if X is a SUBTYPE of Y, then List[X] is a SUBTYPE of List[Y]
   *    * Practical application:
   *      (1) Nil extends List[Nothing]
   *      (2) Nothing is a SUBTYPE of all types
   *      (3) THEN: Nil can be considered a List[Int], a List[Double], and so on
   */

  /*
   * The Linked List Companion Object:
   *  - Kind of a convention in Scala
   *  - object with the same name as the data type (in this case List)
   *    where we put various convenience functions for creating or working with values of the data type
   */
  object List {
    def sum(ns: List[Int]): Int = ns match {
      case Nil        => 0
      case Cons(x,xs) => x + sum(xs)
    }

    def product(ns: List[Double]): Double = ns match {
      case Nil         => 1.0
      case Cons(0.0,_) => 0.0
      case Cons(x,xs)  => x * product(xs)
    }

    /*
     * Example of a Variadic Function:
     *  - A* means it accepts zero or more arguments of type A
     *  - Very common and useful idiom for ADTs, since it allows:
     *      List(1,2,3,4)
     *  - The special _* type annotation allows us to pass a Seq to a variadic method
     */
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  /*
   * EXERCISE 3.1: Guess the result of the following match expression
   */
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x                //(1)
    case Nil                                   => 42               //(2)
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y            //(3)
    case Cons(h, t)                            => h + List.sum(t)  //(4)
    case _                                     => 101              //(5)
  }

  /*
   * (3), (4) and (5) will match, but since (3) is the first one...:
   *  RESULT: x + y = 1 + 2 = 3
   */


  /*
   * EXERCISE 3.2: Implement the function tail for removing the first element of a List.
   * Note that the function takes constant time.
   * What are different choices you could make in your implementation if the List is Nil?
   * We’ll return to this question in the next chapter.
   */
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil       => Nil
    case Cons(_,t) => t
  }
  // tail(List(1,2,3,4))
  // res6: Chapter3Module.List[Int] = Cons(2,Cons(3,Cons(4,Nil)))


  /*
   * EXERCISE 3.3: Implement the function setHead
   * for replacing the first element of a List with a different value.
   */
  def setHead[A](xs: List[A], x: A): List[A] = xs match {
    case Nil       => Cons(x,Nil)
    case Cons(_,t) => Cons(x,t)
  }
  // setHead(List(4,5,6,7,8,9), 6)
  // res12: Chapter3Module.List[Int] = Cons(6,Cons(5,Cons(6,Cons(7,Cons(8,Cons(9,Nil))))))


  /*
   * EXERCISE 3.4: Generalize tail to the function drop, which removes the first n elements from a list.
   * Note that this function takes time proportional only to the number of elements being dropped
   * (we don’t need to make a copy of the entire List).
   */
  def drop[A](xs: List[A], n: Int): List[A] = n match {
    case 0 => xs
    case _ => xs match {
      case Nil       => Nil
      case Cons(_,t) => drop(t, n-1)
    }
  }
  // drop(List(1,2,3,4,5,6), 3)
  // res15: Chapter3Module.List[Int] = Cons(4,Cons(5,Cons(6,Nil)))


  /*
   * EXERCISE 3.5: Implement dropWhile, which removes elements from the List prefix
   * as long as they match a predicate.
   *
   * NOTE: Using a 'pattern guard'.
   *       Syntax: add 'if <cond>' after the pattern, before the '=>',
   *               '<cond>' can use any of the variables introduced by the pattern.
   */
  def dropWhile[A](xs: List[A], pred: A => Boolean): List[A] = xs match {
    case Cons(h,t) if (pred(h)) => dropWhile(t,pred)
    case _                      => xs
  }
  // dropWhile(List(1,2,3,4,5,6,7), (x: Int) => x < 5)
  // res5: Chapter3Module.List[Int] = Cons(5,Cons(6,Cons(7,Nil)))


  /*
   * EXERCISE 3.6: Implement init, which returns a List consisting of all but the last element of a List.
   */
  def init[A](xs: List[A]): List[A] = xs match {
    case Cons(h,Nil) => Nil
    case Cons(h,t)   => Cons(h,init(t))
    case Nil         => Nil
  }
  // scala> init(List(1,2,3,4))
  // res7: Chapter3Module.List[Int] = Cons(1,Cons(2,Cons(3,Nil)))


  /*
   * EXERCISE 3.7a: Implement foldRight:
   *  - replaces the constructors of the list, Nil and Cons, with b and f
   *  - must traverse all the way to the end of the list
   *    (pushing frames onto the call stack as it goes)
   *    before it can begin collapsing it.
   *  - not tail-recursive and will result in a StackOverflowError for large lists
   *    (not stack-safe)
   */
  def foldRight[A,B](as: List[A], b: B)(f: (A,B) => B): B = as match {
    case Nil        => b
    case Cons(a,xs) => f(a, foldRight(xs,b)(f))
  }


  /*
   * EXERCISE 3.7b: Write sum and product using foldRight
   */
  def sumRight(ns: List[Int]): Int =
    foldRight(ns,0)(_+_)
  // scala> sumRight(List(1,2,3,4,5))
  // res112: Int = 15

  def productRight(ns: List[Double]): Double =
    foldRight(ns,1.0)(_*_)
  // scala> productRight(List(1.0,2.0,3.0))
  // res113: Double = 6.0


  /*
   * EXERCISE 3.9: Implement length, which computes the length of a list using foldRight.
   */
  def length[A](xs: List[A]): Int =
    foldRight(xs,0)((_,acc) => acc + 1)
  // scala> length(List(1,2,3,4,5))
  // res83: Int = 5


  /*
   * EXERCISE 3.10: Implement foldLeft, a tail-recursive (stack-safe) fold
   */
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], b: B)(f: (B, A) => B): B = as match {
    case Nil => b
    case Cons(a,xs) => foldLeft(xs,f(b,a))(f)
  }


  /*
   * EXERCISE 3.11: Write sum, product, and length using foldLeft
   */
  def sumLeft(xs: List[Int]): Int =
    foldLeft(xs,0)(_+_)
  // scala> sumLeft(List(1,2,3,4,5))
  // res97: Int = 15

  def productLeft(xs: List[Int]): Int =
    foldLeft(xs,1)(_*_)
  // scala> productLeft(List(1,2,3,4,5))
  // res100: Int = 120

  def lengthLeft[A](xs: List[A]): Int =
    foldLeft(xs,0)((acc,_) => acc + 1)
  // scala> lengthLeft(List(1,2,3,4,5))
  // res104: Int = 5


  /*
   * EXERCISE 3.12: Implement reverse using a fold
   */
  def reverse[A](xs: List[A]): List[A] =
    foldLeft(xs,List[A]())((acc,a) => Cons(a,acc))
  // scala> reverse(List(1,2,3,4,5))
  // res118: Chapter3Module.List[Int] = Cons(5,Cons(4,Cons(3,Cons(2,Cons(1,Nil)))))


  /*
   * EXERCISE 3.14: Implement append in terms of either foldLeft or foldRight
   */
  def appendRight[A](as1: List[A], as2: List[A]): List[A] =
    foldRight(as1,as2)((a1,acc) => Cons(a1,acc))
  // scala> appendRight(List(1,2,3), List(4,5,6))
  // res10: Chapter3Module.List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil))))))


  /*
   * EXERCISE 3.15: Implement flatten, which concatenates a list of lists into a single list.
   * Its runtime should be linear in the total length of all lists
   */
  def flatten[A](xss: List[List[A]]): List[A] =
    foldRight(xss, Nil: List[A])(appendRight)
  // scala> flatten(List(List(1,2,3),List(4,5,6),List(7,8,9)))
  // res28: Chapter3Module.List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Cons(7,Cons(8,Cons(9,Nil)))))))))


  /*
   * EXERCISE 3.16: Implement a function that adds 1 to every integer in a list
   */
  def addOne(ns: List[Int]): List[Int] =
    foldRight(ns, Nil: List[Int])((n,acc) => Cons(n+1,acc))
  // scala> addOne(List(2,4,6,8))
  // res37: Chapter3Module.List[Int] = Cons(3,Cons(5,Cons(7,Cons(9,Nil))))


  /*
   * EXERCISE 3.17: Implement a function that turns each value in a List[Double] into a String.
   * You can use the expression d.toString to convert some d: Double to a String
   */
  def toListOfStrings(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String])((d,acc) => Cons(d.toString, acc))
  // scala> toListOfStrings(List(1.0,2.0,3.5))
  // res40: Chapter3Module.List[String] = Cons(1.0,Cons(2.0,Cons(3.5,Nil)))


  /*
   * EXERCISE 3.18: Implement a map function that
   * generalizes modifying each element in a list while maintaining the structure of the list
   */
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a,bs) => Cons(f(a), bs))
  // scala> map(List(1,2,3))(_+5)
  // res2: Chapter3Module.List[Int] = Cons(6,Cons(7,Cons(8,Nil)))


  /*
   * EXERCISE 3.19: Implement a filter function that
   * removes elements from a list unless they satisfy a given predicate
   */
  def filter[A](as: List[A])(pred: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a,acc) => if (pred(a)) Cons(a, acc) else acc)
  // scala> filter(List(1,2,3,4,5,6,7,8))(_ % 2 == 1)
  // res7: Chapter3Module.List[Int] = Cons(1,Cons(3,Cons(5,Cons(7,Nil))))


  /*
   * EXERCISE 3.20: Implement a flatMap function that works like map except that
   * the function given will return a list instead of a single result, and
   * that list should be inserted into the final resulting list
   */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((a,bs) => appendRight(f(a), bs))
  // scala> flatMap(List(1,2,3))(i => List(i,i))
  // res18: Chapter3Module.List[Int] = Cons(1,Cons(1,Cons(2,Cons(2,Cons(3,Cons(3,Nil))))))

  def flatMap2[A,B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))
  // scala> flatMap2(List(1,2,3))(i => List(i,i))
  // res21: Chapter3Module.List[Int] = Cons(1,Cons(1,Cons(2,Cons(2,Cons(3,Cons(3,Nil))))))


  /*
   * EXERCISE 3.21: Implement filter in terms of flatMap
   */
  def filter2[A](as: List[A])(pred: A => Boolean): List[A] =
    flatMap(as)(a => if (pred(a)) List(a) else Nil)
  // scala> filter(List(1,2,3,4,5,6,7,8))(_ % 2 == 1)
  // res2: Chapter3Module.List[Int] = Cons(1,Cons(3,Cons(5,Cons(7,Nil))))


  /*
   * EXERCISE 3.22: Implement a function that accepts two lists
   * and constructs a new list by adding corresponding elements
   */
  def addLists(ns1: List[Int], ns2: List[Int]): List[Int] = (ns1,ns2) match {
    case (Nil, _)                   => Nil
    case (_, Nil)                   => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addLists(t1,t2))
  }
  // scala> addLists(List(1,2,3), List(4,5,6))
  // res15: Chapter3Module.List[Int] = Cons(5,Cons(7,Cons(9,Nil)))


  /*
   * EXERCISE 3.23: Generalize the function you just wrote so that
   * it's not specific to integers or addition.
   * Name your generalized function zipWith.
   */
  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] = (as,bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case(Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }
  // scala> zipWith(List(1,2,3), List(4,5,6))(_+_)
  // res24: Chapter3Module.List[Int] = Cons(5,Cons(7,Cons(9,Nil)))


  /*
   * EXERCISE 3.24: Implement hasSubsequence for checking whether a List contains
   * another List as a subsequence.
   * Note: Any two values x and y can be compared for equality in Scala using the expression x == y.
   */
  // EXPLANATION:
  //  There are 2 'true' cases:
  //    (1) Both lists are Nil
  //        case(as=Nil,sub=Nil)
  //    (2) At one point both lists start with the same element and that's the case until we exhaust the sub list
  //        case(as,sub) start with the same element and keep matching until the end of sub
  def hasSubsequence[A](as: List[A], sub: List[A]): Boolean = (as, sub) match {
    case (Nil, sub) if sub == Nil          => true
    case (Nil, sub) if sub != Nil          => false
    case (_, _)     if startsWith(as, sub) => true
    case (Cons(_,t), sub)                  => hasSubsequence(t,sub)
  }

  def startsWith[A](as: List[A], sub: List[A]): Boolean = (as,sub) match {
    case (_, Nil)                               => true
    case (Cons(h1,t1), Cons(h2,t2)) if h1 == h2 => startsWith(t1,t2)
    case _                                      => false
  }
  // scala> hasSubsequence(List(1,2,3,4), List(2,3))
  // res2: Boolean = true
  //
  // scala> hasSubsequence(List(1,2,3,4), List(2,4))
  // res3: Boolean = false


  /*
   * The Tree Algebraic Data Type (ADT) parameterized on type A, with 2 data constructors:
   *  - Leaf(value)          (we can construct an empty list with Nil)
   *  - Branch(left,right)   (we can construct a non-empty list with Cons(head,tail))
   */
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

}

