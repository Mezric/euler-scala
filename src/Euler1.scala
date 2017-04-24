

object Euler1 {
  
  def main(args:Array[String]){
    
    val list = 1 until 1000
    
    println(list
      .filter((x:Int) => (x%3==0) || (x%5==0))
      .fold(0)(_+_)
    )
        
    
  }
}