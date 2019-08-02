

import java.io._

import scala.io._

object Result {

  /*
   * Complete the 'dynamicArray' function below.
   *
   * The function is expected to return an INTEGER_ARRAY.
   * The function accepts following parameters:
   *  1. INTEGER n
   *  2. 2D_INTEGER_ARRAY queries
   */

  def dynamicArray(n: Int, queries: Array[Array[Int]]): Array[Int] = {
    // Write your code here

    Array[Int]()
  }

}

object Solution {
  def main(args: Array[String]) {
    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val firstMultipleInput = StdIn.readLine.replaceAll("\\s+$", "").split(" ")

    val n = firstMultipleInput(0).toInt

    val q = firstMultipleInput(1).toInt

    val queries = Array.ofDim[Int](q, 3)

    for (i <- 0 until q) {
      queries(i) = StdIn.readLine.replaceAll("\\s+$", "").split(" ").map(_.trim.toInt)
    }

    val result = Result.dynamicArray(n, queries)

    printWriter.println(result.mkString("\n"))

    printWriter.close()
  }
}
