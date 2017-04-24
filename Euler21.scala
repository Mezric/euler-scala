import scala.math.BigInt;

object Euler21 {
  def main(args:Array[String]){
    
    def d(n:Int):Int = {
      val list = (1 until n)
      list
        .filter((m:Int) => n%m==0)
        .fold(0)(_+_)
    }
    
    val list = (1 until 10000)
    println(d(284))
    println(list
      .filter((n:Int) => d(n) != n && d(d(n)) == n)
      .fold(0)(_+_)
     )
    
  }
}