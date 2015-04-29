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

}

