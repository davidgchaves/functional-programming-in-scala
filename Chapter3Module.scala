/*
 * Load Chapter3Module.scala on the REPL by simply:
 *  scala> :load Chapter3Module.scala
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

}

