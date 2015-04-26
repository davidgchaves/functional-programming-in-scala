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

}

