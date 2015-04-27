/*
 * Load Chapter2Module.scala on the REPL by simply:
 *  scala> :load Chapter2Module.scala
 */
object Chapter2Module {

  /*
   * EXERCISE 2.1: Write a recursive function to get the nth Fibonacci number
   * The first two Fibonacci numbers are 0 and 1.
   * The nth number is always the sum of the previous two.
   * The sequence begins 0, 1, 1, 2, 3, 5.
   * Your definition should use a local tail-recursive function.
   */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, m: Int, acc: Int): Int = n match {
      case 0 => m
      case _ => loop(n-1, acc, m+acc)
    }

    loop(n, 0, 1)
  }

  /*
   * EXERCISE 2.2: Implement isSorted, which
   * checks whether an Array[A] is sorted according to a given comparison function.
   * NOTE1: I don't like how this exercise 'uses' ordered, very ambiguous.
   * NOTE2: I've picked into the hint and answer and they use a much better gt (greater than) function,
   *        which I think clarifies the exercise a bit, but it's not clear if we are talking about desc or asc
   * NOTE3: Ok, it's descending, which is kind of awkard, because when gt(as(n),as(n+1)) is true, then we say isSorted is false... whatever
   */
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else gt(as(n), as(n+1)) match {
        case true => false
        case _    => loop(n+1)
      }
    }

    loop(0)
  }

  /*
   * EXERCISE 2.3: Implement curry,
   * which converts a function f of two arguments into a function of one argument that partially applies f.
   */
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a,b)

  /*
   * EXERCISE 2.4: Implement uncurry, which reverses the transformation of curry.
   * Note that since => associates to the right, A => (B => C) can be written as A => B => C.
   * Just like in Haskell with ->
   */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  /*
   * EXERCISE 2.5: Implement compose, the higher-order function that composes two functions,
   * by feeding the output of one function to the input of another function
   */
   def compose[A,B,C](f: B => C, g: A => B): A => C =
     (a: A) => f(g(a))

}

