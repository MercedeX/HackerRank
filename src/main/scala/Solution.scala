

object Solution {

  def queensAttack(n: Int, k: Int, r_q: Int, c_q: Int, obstacles: Array[Array[Int]]): Int = {
    val locations = obstacles.map(x => (x.head, x.tail.head)).toList
    // prepare board
    var board = Array.ofDim[Char](n, n)
    board(r_q)(c_q) = 'q'
    obstacles.foreach(x => board(x(0))(x(1)) = 'x')
    0
  }
  def getMoneySpent(keyboards: Array[Int], drives: Array[Int], b: Int): (Int, Int) = {

    val K = keyboards.toList
    val M = drives.toList
    val B = b

    val res = K
      .filter(x => B - x >= M.min)
      .map(x => (x, M.filter(y => y <= B - x).max))

    var res2 = if(res.isEmpty)
      None
    else
      Some(res.minBy(x => B - (x._1 + x._2)))

    res2 match{
      case Some(x) => x
      case None => (-1, -1)
    }
  }
  def Int2Str(n: Int): String = {
    val digits = Map(
      0-> '0',
      1-> '1',
      2-> '2',
      3-> '3',
      4-> '4',
      5-> '5',
      6-> '6',
      7-> '7',
      8-> '8',
      9-> '9'
    )
    var tmp = n
    var sb = new StringBuilder()
    while(tmp> 0){
      sb.append( digits(tmp % 10))
      tmp = tmp/10
    }
    sb.foldRight("")((p,n)=> n :+ p)
  }
  def findDigits(n: Int): Int = {
    val count0 = Int2Str(n).toList.map(x => x.asDigit).filter(_>0).count(x => n % x ==0)
    count0
  }
  def timeInWords(h: Int, m: Int): String = {
    val units = Map(0-> "twelve", 1-> "one", 2->"two", 3-> "three", 4-> "four", 5-> "five",
      6->"six", 7-> "seven", 8-> "eight", 9-> "nine", 10-> "ten",
      11-> "eleven", 12-> "twelve", 13-> "thirteen", 14-> "fourteen", 15-> "quarter",
      16-> "sixteen", 17-> "seventeen", 18-> "eighteen", 19-> "ninteen")
    val tens = Map(2-> "twenty", 3-> "thirty", 4-> "forty", 5-> "fifty")

    val tTime = if(m>30) ((h%12)+1, 60-m) else (h,m)
    val hours = units(tTime._1)
    val minutes = tTime._2 match {
      case x if x < 20 => units(x)
      case x  =>  s"${tens(x/10)} ${units(x%10)}"
    }
    tTime._2 match {
      case 0 => s"$hours o' clock"
      case x if m<30 =>  if(x==1) s"$minutes minute past $hours" else if(x==15)  s"$minutes past $hours" else s"$minutes minutes past $hours"
      case x if m==30 => s"half past $hours"
      case x if m>30 => if(x==1) s"$minutes minute to $hours" else if(x==15)  s"$minutes to $hours" else s"$minutes minutes to $hours"
    }
  }

}
